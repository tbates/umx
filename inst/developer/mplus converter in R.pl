# Based on MplusParser.pm  by Joel S Steele Version: 0.3b
# http://openmx.psyc.virginia.edu/thread/236

# TODO handle | style syntax. 

sub new{
	my $class = shift;
	my $file = shift || 0;
	my $self = {};
	bless $self, $class;
	# a bit of preprocessing
	if($file){
		if($self->slurpMplus($file)){
			$self->parseMplus();
		}
	}
				 # Any number of characters A-Z 0-9 and the underscore
				 # |    with either an @ or * symbol (? = maybe)
				 # |     |     with a numer following, possibly negative allowing for the decimal point(? = maybe)
				 # |     |      |          with a starting '(' (? = maybe)
				 # |     |      |           | and anything not a ')' (? = maybe)
				 # |     |      |           |  |       followed by an ending ')' (? = maybe)
				 # |     |__    |______     |  |___     |
				 # |     |  |   |      |    |  |   |    | 
	$self->{'re_pattern'} =	'(\w+)(?:@|\*)?([\d\-\.]*)?\(?([^\)]*)?\)?';
	# better than a split in case the starting values are negative
	$self->{'range_pattern'} = '([^\-]+)\-(.*)$';
	return $self;
}

sub slurpMplus{
	my $self = shift;
	my $infile = shift;
	local undef $/; # no input record separator... gimme the whole thing as a string!!!
	unless(open(FH, $infile)){ 
		warn "unable to $infile for reading $!\n"; 
		return 0;
	};
	my $filetxt = <FH>; #tasty...
	close(FH);
	$self->{'filetxt'} = $filetxt;
	return 1;
}

sub outputModelTxt{
	my $self = shift;

	# first we set up the templates
	# --------------------------------------------------------------------- #
	# Just to keep things straight here are the
	# labels from the above template that need
	# to be filled in from the Mplus script
	# 	
	# 	filename = name of the input file
	# 	MISSING = missing values
	# 	datalabels = variable names from the file
	# 	uselabeles = variables in the usevar statement
	# 	model_name = what R will call this object
	# 	model_label = the human readable version of the above
	# 	data_type = raw, covariance, summary, etc
	# 	means_vect = vector of means
	# 	numobs = number of observations
	# 	mvars = manifest variables
	# 	lvars = latent variables
	# 	paths = a string representation of the specified paths
	#
	# --------------------------------------------------------------------- #
	my $modelTMPLT = qq'# change this to fit your needs
# not everyone likes CSV.
input.df <- read.csv("<TMPL_VAR NAME="filename">",header=FALSE<TMPL_IF NAME="missing">,na.strings="<TMPL_VAR NAME="missing">"</TMPL_IF>);
names(input.df) <- c(<TMPL_VAR NAME="datalabels">);
usedata <- input.df[,c(<TMPL_VAR NAME="uselabels">)];
	
<TMPL_VAR NAME="model_name"> <- mxModel(name="<TMPL_VAR NAME="model_label">",
	type="RAM",
	mxData(observed=usedata, 
	       type=<TMPL_IF NAME="data_type">"<TMPL_VAR NAME="data_type">"<TMPL_ELSE>"raw"</TMPL_IF><TMPL_IF NAME="means_vect">,
	       means=c("<TMPL_VAR NAME="means_vect">")</TMPL_IF><TMPL_IF NAME="numobs">,
	       numObs=<TMPL_VAR NAME="numobs"></TMPL_IF>),
	manifestVars=c(<TMPL_VAR NAME="mvars">),
	<TMPL_IF NAME="lvars">latentVars=c(<TMPL_VAR NAME="lvars">),</TMPL_IF>
	<TMPL_VAR NAME="paths">
);
	';

	# --------------------------------------------------------------------- #
	# More lables this time for the path template
	# 	
	# 	pth_fromvars = the beginning of the path(s)
	# 	pth_tovars = where the path(s) end
	# 	pth_arrows = the number of arrows needed for the path(s)
	# 	pth_free = free or fixed path(s)
	# 	pth_values = starting or fixed value(s) of the path(s)
	# 	pth_labels = parameter name(s) for the path(s)
	# 	
	# --------------------------------------------------------------------- #
	my $pathTMPLT = qq'
	mxPath(	from=c(<TMPL_VAR NAME="pth_fromvars">), 
		<TMPL_IF NAME="pth_tovars">to=c(<TMPL_VAR NAME="pth_tovars">),</TMPL_IF>
		arrows=<TMPL_VAR NAME="pth_arrows">, 
		free=c(<TMPL_VAR NAME="pth_free">),
		values=c(<TMPL_VAR NAME="pth_values">),
		labels=c(<TMPL_VAR NAME="pth_labels">)
		)';

	my $model_tmpl = HTML::Template->new(scalarref => \$modelTMPLT, die_on_bad_params=>0);
	my $path_tmpl = HTML::Template->new(scalarref => \$pathTMPLT, die_on_bad_params=>0);
	
	# for cleaner easier output
	my @paths;
	for(@{$self->{'pathoutput'}}){
		$path_tmpl->param($_); #load the current path parameters
		push @paths, $path_tmpl->output();
		$path_tmpl->clear_params(); #clean up after.
	}
	$self->{'output'}->{'paths'} = join(",\n", @paths);
	$model_tmpl->param($self->{'output'}); #load the full model
	return($model_tmpl->output());

}

sub parseMplus{
	my $self = shift;
	my $filetxt = $self->{'filetxt'};
	my %chunks;
	my $label;

	# clear out any comments
	$filetxt =~ s/!.*?$//mg;
	# blank lines
	$filetxt =~ s/^\s*$//mg;
	# and any leading whitespace.
	$filetxt =~ s/^\s+//mg;
	
	#set the input record separator to newline
	local $/ = "\n";
	open FH, "<", \$filetxt; #open the string as a filehandle
	while(<FH>){ #read from it!
		my $ltxt;
		#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
		# this only matches for the basic input commands and will 
		# not match for separate models as in multiple
		# group analysis or for model constraints.
		#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
		if(/^[a-z]+:[^\/\\]/i){ #colons are special in Mplus but we need to make sure we are not matching a physical drive.
			($label,$ltxt) = $_ =~ /^([a-z]+):(.*?)$/i; #capture the section label based on the first colon in the line.
			$label = lc($label);
			# since Mplus commands can be shortened to 4 characters we will
			# use the first 4 for any command just to be safe
			$label = join('',(split(//,$label))[0..3]);
		}else{
			$ltxt = $_;
		}
		#clean up whitespace
		$ltxt =~ s/^\s+//; #front...
		chomp($ltxt);	   #back... side to side.	
		#only if it matches something other than white-space
		push @{$chunks{$label}},$ltxt if($ltxt =~ /\w/);
	}
	close(FH);
	# save this guy for later.
	$self->{'script_parts'} = \%chunks;
}

# split things up within each section
sub processMplus{
	my $self = shift;
	$self->_MplusData();
	$self->_MplusTitle();
	$self->_MplusVars();
	$self->_MplusModel();

}

sub _MplusData{
	my $self = shift;
	# work with the data chunks
	my $data_arr = $self->{'script_parts'}->{'data'};
	my $str = join('',@{$data_arr}); # bring it all back together
	# search for the filename
	my ($file) = $str =~ /FILE\s+IS|=\s?([^;]+);/si;
	if($file){
		chomp($file);
		$self->{'output'}->{'filename'} = $file;
	}
	# the type if present
	if($str =~ /TYPE/){
		my ($type) = $str =~ /TYPE\s+IS|=\s*([^;]+);/i;
		if(defined $type && $type =~ /cov/i){
			$self->{'output'}->{'data_type'} = 'cov';
		}elsif(defined $type && $type =~ /corr/i){
			$self->{'output'}->{'data_type'} = 'cor';
		}
	}
	# and the number of observations if present
	if($str =~ /NOBS/){
		# this is crufty...
		my ($chunk) = $str =~ /NOBS([^;]+);/i; #weak.
		my ($nobs) = $chunk =~ /(\d+)/; #lame.
		if(defined $nobs && $nobs > 0){
			$self->{'output'}->{'numobs'} = $nobs;
		}
	}
}

sub _MplusVars{
	my $self = shift;
	my $vars_arr = $self->{'script_parts'}->{'vari'};
	my $str = join(' ',@{$vars_arr});
	# assuming everyone ends things in semicolons
	# we split each section up by the semicolon,
	# leaving the NAMES, USEVARIABLES and MISSING statements
	# to process as chunks of text
	my @secs = split(/;/,$str); 

				# look for the NAMES statement in the resultant chunks of text
	my @datalabels = do{	my @ns = grep(/NAMES/i,@secs); 
				$ns[0] =~ s/ARE/=/; # replace ARE with = if present
				my @v = split(/=/,$ns[0]); # now split on the = sign
				# this will leave the text 'NAMES' as $v[0] and everything else
				# as $v[1] in the @v array.
				my @n = split(/\s+/, $v[1]); #split up the variable names based on
				# one or more spaces in the text.
				my @arr;
				for my $n (@n){ #loop through each element from the above split
					# this is to handle ranges in specification
					if($n =~ /\-/){ # look for the - character signifying a range
						my ($start, $stop) = split(/\-/,$n); # split it up on the - and 
						# capture the starting and stopping values of the range.
						# Since this is generally of the form A1-A20 or something similar
						# we need to separate the leading character from the numerical parts
						my ($pre1, $num1) = $start =~ /([a-zA-Z_]+)(\d+)/; 
						my ($pre2, $num2) = $stop =~ /([a-zA-Z_]+)(\d+)/;
						if($pre1 eq $pre2){ # Make sure the non numeric parts match up.
							for my $c ($num1 .. $num2){ # us the start and stop numerical parts to 
								# create the range... here we just use one of the prefixes since
								# the both match.
								push @arr, $pre1.$c;
							}
						}
					}elsif($n =~ /\w+/){ #if there was no range make sure the element matches something
						push @arr, $n;
					}
				}
				@arr; # return the results
			};
						# quote for R's sake
	$self->{'output'}->{'datalabels'} = join(',',(map{qq~"$_"~;}@datalabels));

	my @uselabels = do{	my @ns = grep(/USEVAR/i,@secs);
				$ns[0] =~ s/ARE/=/;
				my @v = split(/=/,$ns[0]);
				my @n = split(/\s+/, $v[1]);
				my @arr;
				for my $n (@n){
					# this is to handle ranges in specification
					if($n =~ /\-/){
						my ($start, $stop) = split(/\-/,$n);
						my ($pre1, $num1) = $start =~ /([a-zA-Z_]+)(\d+)/; 
						my ($pre2, $num2) = $stop =~ /([a-zA-Z_]+)(\d+)/;
						if($pre1 eq $pre2){
							for my $c ($num1 .. $num2){
								push @arr, $pre1.$c;
							}
						}
					}elsif($n =~ /\w+/){
						push @arr, $n;
					}
				}
				@arr;
			};
	$self->{'output'}->{'uselabels'} = join(',',(map{qq~"$_"~;}@uselabels));

	$self->{'output'}->{'mvars'} = $self->{'output'}->{'uselabels'};

	# missing values???
	if(grep(/MISSING/i,@secs)){
		my @ns = grep(/MISSING/i,@secs);
		$ns[0] =~ s/ARE/=/;
		my @v = split(/=/,$ns[0]);
		my $mvalstr;
		if($v[1] =~ /(\.)/ || $v[1] =~ /(\*)/){
			$mvalstr = 'c("'.$1.'")';
		}elsif($v[1] =~ /\([^\)]+\)/){
			my @mvals = $v[1] =~ /\(([^\)]+)\)/g;
			$mvalstr = join(' ',@mvals);
			$mvalstr =~ s/\s+/,/g;
			$mvalstr =~ s/\-/../g;
			$mvalstr = 'c('.$mvalstr.')';
		}
		$self->{'output'}->{'missing'} = $mvalstr;
	}
}


sub _MplusModel{
	my $self = shift;
	
	# working with the model part, here shortened to mode
	# because of the four character shortcuts allowed in Mplus
	my $model_arr = $self->{'script_parts'}->{'mode'};
	my $str = join(' ',@{$model_arr}); # bring it all back together
	my %part_hash;
	my @mparts = split(';',$str); # split the big string on semicolons
	
	# search through the resultant chunks for 
	@{$part_hash{'bylist'}} = grep(/ BY /i, @mparts); 		# BY statements
	@{$part_hash{'onlist'}} = grep(/ ON /i, @mparts); 		# ON statements
	@{$part_hash{'withlist'}} = grep(/ WITH /i, @mparts);		# WITH statements
	# notice we left some space around each of these so they wouldn't match the ones below.
	# shortcut support
	@{$part_hash{'ponlist'}} = grep(/ PON /i, @mparts);		# PON statements
	@{$part_hash{'pwithlist'}} = grep(/ PWITH /i, @mparts);		# PWITH statements
	@{$part_hash{'meanslist'}} = grep(/\[[^\]]+\]/, @mparts);	# []'s so means
	# anything not accompanied by an ON,PON,BY,WITH,PWITH or []'s is a variance
	@{$part_hash{'varslist'}} = grep{$_ !~ /\bP?ON|BY|P?WITH\b/ && $_ !~ /\[|\]/}@mparts;
	$self->{'model_parts'} = \%part_hash;

	$self->_modelBY(); # do the BY first to collect latent variables
	$self->_modelON();
	$self->_modelWITH();
	$self->_modelPON();
	$self->_modelPWITH();
	$self->_modelVariances();
	$self->_modelMeans();
	if(exists $self->{'latents'}){
		$self->{'output'}->{'lvars'} = join(',',(map{qq~"$_"~}(keys %{$self->{'latents'}})));
	}
}

sub _modelBY{
	my $self = shift;
	$self->_modelParseArrows('bylist','BY','L_'); # Lambda
}

sub _modelON{
	my $self = shift;
	$self->_modelParseArrows('onlist','ON','B_'); # Beta
}

sub _modelWITH{
	my $self = shift;
	$self->_modelParseArrows('withlist','WITH','C_'); # Covariance
}

# it is ALL possible... how much time to invest is the question.
# We don't want to give the following elements the same labels as those
# above so we add a leading 'p'
sub _modelPON{
	my $self = shift;
	$self->_modelParseArrows2('ponlist','PON','pB_'); # paired Beta
}

sub _modelPWITH{
	my $self = shift;
	$self->_modelParseArrows2('pwithlist','PWITH','pC_'); # paired Covariance
}

sub _modelParseArrows{
	my $self = shift;
	my $lst = shift;
	my $split = shift;
	my $label = shift;
	return 0 if(!exists $self->{'model_parts'}->{$lst});
	my $list = $self->{'model_parts'}->{$lst};
	my @paths;
	# A range could include manifest and latent variables. Check both if we have them.
	# This is why we run the BY statements first.
	# Trust me there is a ternary in there.
	my @mvars = (exists $self->{'latents'})?
		((map{s/"//g;$_;}(split /,/,$self->{'output'}->{'mvars'})),(keys %{$self->{'latents'}})):
		(map{s/"//g;$_;}(split /,/,$self->{'output'}->{'mvars'}));
	chomp(@mvars);
	my %mvars = map{($_,1);}@mvars;
	my $pcount = 0;
	for(@{$list}){
		# using the supplied split characters create 
		# a left hand side and right hand side of the statement
		my ($lhs, $rhs) = split(/$split/,$_);
		# cuddle things up to each other for free, fixed and labeled stuff
		$lhs =~ s/\s//g; # remove a single space globally
		$rhs =~ s/^\s+//; # remove leading spaces
		$rhs =~ s/\s*\@\s*/@/g; # remove spaces around the @ symbols
		$rhs =~ s/\s*\(/(/g; # remove spaces around ()'s, well at least the first one
		$rhs =~ s/\s*\*\s*/*/g; # remove spaces around the * symbols
		my %tmp;
		my (@rhs, @labs, @frees, @vals);
		#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
		# Here we only look for right hand side ranges. This may 
		# need updating if people regress a list of variables 
		# onto a single variable... hmmmmm.
		#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
		if($rhs =~ /\-\s*[^\d\.]/){ # do we have a range here?
			#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
			# this could really get mucky if people regularly abuse the
			# range operator in Mplus. Hopefully the only define ranges 
			# with the same prefixes.
			#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
			# split up the range in to start and stop values.
			my ($start, $stop) = $rhs =~ /$self->{'range_pattern'}/o;
			# here we extract parts of the right hand side
			my ($rhs1, $val1, $lab1) = $start =~ /$self->{'re_pattern'}/o;
			my ($rhs2, $val2, $lab2) = $stop =~ /$self->{'re_pattern'}/o;
			my $keep = 0;
			# Schwartzian Transformation. 
			# Read from the bottom up.
			# We need to sort the varaibles we currently have since we are not sure they all came in order.
			# So we split up each one by its leading characters and possible trailing digits
			# We then store those values along with the original value
			foreach my $cv(map{$_->[2]}					# Step 7: return the copy of the original based on the sort below.
					sort{$a->[0] cmp $b->[0] 			# Step 5: Sort on the leading characters
						|| 
					     $a->[1] <=> $b->[1];}			# Step 6: or sort on the digits
	     				map{ 	
						my $v = $_; 				# Step 2: set a temp variable to the incoming value $_
						my ($w, $d) = $v =~ /(\D+)(\d+)?/; 	# Step 3: capture the leading characters and the trailing digits 
						(defined $d)?[$w, $d, $v]:[$v,$v,$v];	# Step 4: store the leading characters, digits and the original, 
											#         or just three copies of the original
					}keys %mvars					# Step 1: for each variable
				){ 
					# loop through each variable already collected from the sorted list above
				if($cv eq $rhs1){ # is this the start of our range?
					$keep = 1; 
				}elsif($cv eq $rhs2){ # or the end of our range?
					$keep = 0;
					push @rhs, $cv;
					# check if the regexp caught anything otherwise default
					push @vals, (defined $val2 && $val2 =~ /\S/)?$val2:'0';
					push @labs, (defined $lab2 && $lab2 =~ /\S/)?$lab2:$label.$pcount++;
					push @frees, ($stop =~ /@/)?'FALSE':'TRUE'; #is there an @ on the end of this range?
				}
				if($keep){
					push @rhs, $cv;
					push @vals, (defined $val2 && $val2 =~ /\S/)?$val2:'0';
					push @labs, (defined $lab2 && $lab2 =~ /\S/)?$lab2:$label.$pcount++;
					push @frees, ($stop =~ /@/)?'FALSE':'TRUE';
				}
			}
		}else{
			for my $c (split(/\s+/,$rhs)){
				my ($rhs1, $val, $lab) = $c =~ /$self->{'re_pattern'}/o;
				push @rhs, $rhs1;
				push @vals, (defined $val && $val =~ /\S/)?$val:'0';
				push @labs, (defined $lab && $lab =~ /\S/)?$lab:$label.$pcount++;
				push @frees, ($c =~ /@/)?'FALSE':'TRUE';
			}
		}
		# check the arrow directions
		if($split eq "ON"){
			$tmp{'pth_tovars'} = qq~"$lhs"~;
			$tmp{'pth_fromvars'} = join(',',(map{qq~"$_"~}@rhs));
		}else{
			# collect latent variables
			if($split eq "BY"){
				$self->{'latents'}->{$lhs}++;
				$mvars{$lhs}++;
			}
			$tmp{'pth_fromvars'} = qq~"$lhs"~;
			$tmp{'pth_tovars'} = join(',',(map{qq~"$_"~}@rhs));
		}
		# check arrow numbers
		if($split eq "WITH"){
			$tmp{'pth_arrows'} = 2;
		}else{
			$tmp{'pth_arrows'} = 1;
		}
		$tmp{'pth_free'} = join(',',@frees);
		$tmp{'pth_values'} = join(',',@vals);
		# we do the quoting for R's sake
		$tmp{'pth_labels'} = join(',',(map{qq~"$_"~}@labs));
		push @paths,\%tmp;
	}
	push @{$self->{'pathoutput'}}, $_ for(@paths);

}

# here we handle PON's and PWITH's for paired arrow specification shortcuts.
sub _modelParseArrows2{
	my $self = shift;
	my $lst = shift;
	my $split = shift;
	my $label = shift;
	my $list = $self->{'model_parts'}->{$lst};
	return 0 if(!exists $self->{'model_parts'}->{$lst});
	my @paths;
	my $pcount = 0;

	# a range could include manifest and latent variables. Check both if we have them.
	# This is why we run the BY statements first.
	my @mvars = (exists $self->{'latents'})?
		((map{s/"//g;$_;}(split /,/,$self->{'output'}->{'mvars'})),(keys %{$self->{'latents'}})):
		(map{s/"//g;$_;}(split /,/,$self->{'output'}->{'mvars'}));
	chomp(@mvars);
	my %mvars = map{($_,1);}@mvars;
	for(@{$list}){
		my ($lhs, $rhs) = split(/$split/,$_);
		# cuddle things up to each other for free, fixed and labeled stuff
		$rhs =~ s/^\s+//; # remove leading spaces
		$rhs =~ s/\s*\@\s*/@/g; # remove spaces around the @ symbol
		$rhs =~ s/\s*\(/(/g; # remove spaces around ()'s, well at least the first one
		$rhs =~ s/\s*\*\s*/*/g; # remove spaces around the * symbol
		my (@lhs);
		if($lhs =~ /\-\s*[^\d\.]/){ # do we have a range here?
			my ($start, $stop) = $lhs =~ /$self->{'range_pattern'}/o;
			my $lhs1 = $start; # no labels or free fixin's on this side of things.
			my $lhs2 = $stop;
			$lhs1 =~ s/\s+//g;
			$lhs2 =~ s/\s+//g;
			chomp($lhs1);
			chomp($lhs2);
			my $keep = 0;
			# Schwartzian Transformation. 
			foreach my $cv(map{$_->[2]}
				sort{$a->[0] cmp $b->[0] || $a->[1] <=> $b->[1];} 
				map{my $v = $_;my ($w, $d) = $v =~ /(\D+)(\d+)?/; (defined $d)?[$w, $d, $v]:[$v,$v,$v];}keys %mvars
				){ 
				if($cv eq $lhs1){
					$keep = 1;
				}elsif($cv eq $lhs2){
					$keep = 0;
					push @lhs, $cv;
				}
				if($keep){
					push @lhs, $cv;
				}
			}
		}else{
			for my $c (split(/\s+/,$lhs)){
				push @lhs, $c;
			}
		}

		my (@rhs, @labs, @frees, @vals);
		if($rhs =~ /\-\s*[^\d\.]/){ # do we have a range here?
			my ($start, $stop) = $rhs =~ /$self->{'range_pattern'}/o;
			my ($rhs1, $val1, $lab1) = $start =~ /$self->{'re_pattern'}/o;
			my ($rhs2, $val2, $lab2) = $stop =~ /$self->{'re_pattern'}/o;
			my $keep = 0;
			# Schwartzian Transformation. 
			foreach my $cv(map{$_->[2]}
				sort{$a->[0] cmp $b->[0] || $a->[1] <=> $b->[1];} 
				map{my $v = $_;my ($w, $d) = $v =~ /(\D+)(\d+)?/; (defined $d)?[$w, $d, $v]:[$v,$v,$v];}keys %mvars
				){ 
				if($cv eq $rhs1){
					$keep = 1;
				}elsif($cv eq $rhs2){
					$keep = 0;
					push @rhs, $cv;
					push @vals, (defined $val2 && $val2 =~ /\S/)?$val2:'0';
					push @labs, (defined $lab2 && $lab2 =~ /\S/)?$lab2:$label.$pcount++;
					push @frees, ($stop =~ /@/)?'FALSE':'TRUE'; #is there an @ on the end of this range?
				}
				if($keep){
					push @rhs, $cv;
					push @vals, (defined $val2 && $val2 =~ /\S/)?$val2:'0';
					push @labs, (defined $lab2 && $lab2 =~ /\S/)?$lab2:$label.$pcount++;
					push @frees, ($stop =~ /@/)?'FALSE':'TRUE';
				}
			}
		}else{
			for my $c (split(/\s+/,$rhs)){
				my ($rhs1, $val, $lab) = $c =~ /$self->{'re_pattern'}/o;
				push @rhs, $rhs1;
				push @vals, (defined $val && $val =~ /\S/)?$val:'0';
				push @labs, (defined $lab && $lab =~ /\S/)?$lab:$label.$pcount++;
				push @frees, ($c =~ /@/)?'FALSE':'TRUE';
			}
		}
		my ($numarrows,$from, $to);
		# check arrow numbers
		if($split eq "PWITH"){
			$numarrows = 2;
			$from = 'pth_fromvars';
			$to = 'pth_tovars';
		}else{
			$numarrows = 1;
			$from = 'pth_tovars';
			$to = 'pth_fromvars';
		}
		if($#lhs == $#rhs){
			for my $i (0 .. $#lhs){
				my %p_tmp;
				$p_tmp{$from} = qq~"$lhs[$i]"~;
				$p_tmp{$to} = qq~"$rhs[$i]"~;
				$p_tmp{'pth_arrows'} = $numarrows;
				$p_tmp{'pth_free'} = $frees[$i];
				$p_tmp{'pth_values'} = $vals[$i];
				$p_tmp{'pth_labels'} = qq~"$labs[$i]"~;
				push @paths, \%p_tmp;
			}
		}else{
			print "@lhs\n";
			print "\n===\n";
			print "@rhs\n";
			die "Things don't match up in the $split statement!\nCheck your syntax.";
		}
	}
	push @{$self->{'pathoutput'}}, $_ for(@paths);
}


# these two could be factored out more, but it is only two subs... meh.
sub _modelVariances{
	my $self = shift;
	return 0 if(!exists $self->{'model_parts'}->{'varslist'});
	my @paths;
	# this handles multiple specification prior to the ';'
	my @list = grep{ /\w/ }map{		
		$_ =~ s/^\s+//; # remove leading spaces
		$_ =~ s/\s*\@\s*/@/g; # remove spaces around the @ symbol
		$_ =~ s/\s*\(/(/g; # remove spaces around ()'s, well at least the first one
		$_ =~ s/\s*\*\s*/*/g; # remove spaces around the * symbol
		split(/\s/,$_);
		}@{$self->{'model_parts'}->{'varslist'}};
	my $label = 'V_';
	my $pcount = 0;
	# a range could include manifest and latent variables. Check both if we have them.
	# This is why we run the BY statements first.
	my @mvars = (exists $self->{'latents'})?
		((map{s/"//g;$_;}(split /,/,$self->{'output'}->{'mvars'})),(keys %{$self->{'latents'}})):
		(map{s/"//g;$_;}(split /,/,$self->{'output'}->{'mvars'}));
	chomp(@mvars);
	my %mvars = map{($_,1);}@mvars;
	
	for(@list){
		my (%tmp,@rhs,@vals,@frees,@labs);
		if($_ =~ /\-\s*[^\d\.]/){ # range here?
			my ($start, $stop) = $_ =~ /$self->{'range_pattern'}/o;
			my ($rhs1, $val1, $lab1) = $start =~ /$self->{'re_pattern'}/o;
			my ($rhs2, $val2, $lab2) = $stop =~ /$self->{'re_pattern'}/o;
			my $keep = 0;
			# Schwartzian Transformation. 
			foreach my $cv(map{$_->[2]}
				sort{$a->[0] cmp $b->[0] || $a->[1] <=> $b->[1];} 
				map{my $v = $_;my ($w, $d) = $v =~ /(\D+)(\d+)?/; (defined $d)?[$w, $d, $v]:[$v,$v,$v];}keys %mvars
				){ 
				if($cv eq $rhs1){
					$keep = 1;
				}elsif($cv eq $rhs2){
					$keep = 0;
					push @rhs, $cv;
					push @vals, (defined $val2 && $val2 =~ /\S/)?$val2:'0';
					push @labs, (defined $lab2 && $lab2 =~ /\S/)?$lab2:$label.$pcount++;
					push @frees, ($stop =~ /@/)?'FALSE':'TRUE';
				}
				if($keep){
					push @rhs, $cv;
					push @vals, (defined $val2 && $val2 =~ /\S/)?$val2:'0';
					push @labs, (defined $lab2 && $lab2 =~ /\S/)?$lab2:$label.$pcount++;
					push @frees, ($stop =~ /@/)?'FALSE':'TRUE';
				}
			}
		}else{
			my ($rhs1, $val, $lab) = $_ =~ /$self->{'re_pattern'}/o;
			push @rhs, $rhs1;
			push @vals, (defined $val && $val =~ /\S/)?$val:'0';
			push @labs, (defined $lab && $lab =~ /\S/)?$lab:$label.$pcount++;
			push @frees, ($_ =~ /@/)?'FALSE':'TRUE';
		}
		$tmp{'pth_arrows'} = 2;
		$tmp{'pth_fromvars'} = join(',',(map{qq~"$_"~}@rhs));
		$tmp{'pth_tovars'} = join(',',(map{qq~"$_"~}@rhs)); # I guess this could be dropped since it is implied by OpenMx...
		$tmp{'pth_free'} = join(',',@frees);
		$tmp{'pth_values'} = join(',',@vals);
		$tmp{'pth_labels'} = join(',',(map{qq~"$_"~}@labs));
		push @paths,\%tmp;
		
	}
	push @{$self->{'pathoutput'}}, $_ for(@paths);
}

sub _modelMeans{
	my $self = shift;
	return 0 if(!exists $self->{'model_parts'}->{'meanslist'});
	my @paths;
	# this handles multiple specifications in the same set of []'s
	my @list = grep{ /\w/ }map{		
		$_ =~ s/^\s+//; # remove leading spaces
		$_ =~ s/\s*\@\s*/@/g; # remove spaces around the @ symbol
		$_ =~ s/\s*\(/(/g; # remove spaces around ()'s, well at least the first one
		$_ =~ s/\s*\*\s*/*/g; # remove spaces around the * symbol
		split(/\s/,$_);
		}@{$self->{'model_parts'}->{'meanslist'}};
	my $label = 'M_';
	my $pcount = 0;
	# a range could include manifest and latent variables. Check both if we have them.
	# This is why we run the BY statements first.
	my @mvars = (exists $self->{'latents'})?
		((map{s/"//g;$_;}(split /,/,$self->{'output'}->{'mvars'})),(keys %{$self->{'latents'}})):
		(map{s/"//g;$_;}(split /,/,$self->{'output'}->{'mvars'}));
	chomp(@mvars);
	my %mvars = map{($_,1);}@mvars;

	for(@list){
		$_ =~ s/[\[\]]//g;
		my (%tmp,@rhs,@vals,@frees,@labs);
		if($_ =~ /\-\s*[^\d\.]/){
			my ($start, $stop) = $_ =~ /$self->{'range_pattern'}/o;
			my ($rhs1, $val1, $lab1) = $start =~ /$self->{'re_pattern'}/o;
			my ($rhs2, $val2, $lab2) = $stop =~ /$self->{'re_pattern'}/o;
			my $keep = 0;
			# Schwartzian Transformation. 
			foreach my $cv(map{$_->[2]}
				sort{$a->[0] cmp $b->[0] || $a->[1] <=> $b->[1];} 
				map{my $v = $_;my ($w, $d) = $v =~ /(\D+)(\d+)?/; (defined $d)?[$w, $d, $v]:[$v,$v,$v];}keys %mvars
				){ 
				if($cv eq $rhs1){
					$keep = 1;
				}elsif($cv eq $rhs2){
					$keep = 0;
					push @rhs, $cv;
					push @vals, (defined $val2 && $val2 =~ /\S/)?$val2:'0';
					push @labs, (defined $lab2 && $lab2 =~ /\S/)?$lab2:$label.$pcount++;
					push @frees, ($stop =~ /@/)?'FALSE':'TRUE';
				}
				if($keep){
					push @rhs, $cv;
					push @vals, (defined $val2 && $val2 =~ /\S/)?$val2:'0';
					push @labs, (defined $lab2 && $lab2 =~ /\S/)?$lab2:$label.$pcount++;
					push @frees, ($stop =~ /@/)?'FALSE':'TRUE';
				}
			}
		}else{
			my ($rhs1, $val, $lab) = $_ =~ /$self->{'re_pattern'}/o;
			push @rhs, $rhs1;
			push @vals, (defined $val && $val =~ /\S/)?$val:'0';
			push @labs, (defined $lab && $lab =~ /\S/)?$lab:$label.$pcount++;
			push @frees, ($_ =~ /@/)?'FALSE':'TRUE';
		}
		$tmp{'pth_arrows'} = 1;
		$tmp{'pth_fromvars'} = qq~"one"~;
		$tmp{'pth_tovars'} = join(',',(map{qq~"$_"~}@rhs));
		$tmp{'pth_free'} = join(',',@frees);
		$tmp{'pth_values'} = join(',',@vals);
		$tmp{'pth_labels'} = join(',',(map{qq~"$_"~}@labs));
		push @paths,\%tmp;
		
	}
	push @{$self->{'pathoutput'}}, $_ for(@paths);
}

_MplusTitle
MplusTitle <- function{
	my $self = shift;
	my $title_arr = $self->{'script_parts'}->{'titl'};
	my $title = join('',@{$title_arr});
	if($title){
		chomp($title);
		$self->{'output'}->{'model_label'} = $title;
	}else{
		$self->{'output'}->{'model_label'} = 'Model1';
	}
	$self->{'output'}->{'model_name'} = 'Model1';
}
