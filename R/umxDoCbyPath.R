#' Make a direction of causation model based on umxPath statements
#'
#' @description
#' Makes a direction of causation model with [umxPath()] statements
#'
#' @details
#' See also [umxDoC()]
#' @param var1Indicators The indicators of trait 1
#' @param var2Indicators The indicators of trait 2
#' @param mzData The MZ twin dataframe
#' @param dzData The DZ twin dataframe
#' @param sep  (Default "_T")
#' @param causal (Default TRUE)
#' @param name = "DoC"
#' @param autoRun Default: getOption("umx_auto_run")_
#' @param intervals Whether to run intervals (Default FALSE)
#' @param tryHard  Default "no" (valid = "yes", "ordinal", "search")
#' @param optimizer Whether to set this for this run (Default no))
#' @return - [A direction of causation model with [umxPath()] statements.
#' @export
#' @family Twin Modeling Functions
#' @seealso - [umxDoC()]
#' @md
#' @examples
#' \dontrun{
#' # ================
#' # = 1. Load Data =
#' # ================
#' data(docData)
#' var1 = paste0("varA", 1:3)
#' var2 = paste0("varB", 1:3)
#' tmp = umx_scale_wide_twin_data(varsToScale= c(var1, var2), sep= "_T", data= docData)
#' mzData = subset(docData, zygosity %in% c("MZFF", "MZMM"))
#' dzData = subset(docData, zygosity %in% c("DZFF", "DZMM"))
#' m1 = umxDoCp(var1, var2, mzData= mzData, dzData= dzData, sep = "_T", causal= TRUE)
#' 
#' }
umxDoCp <- function(var1Indicators, var2Indicators, mzData= NULL, dzData= NULL, sep = "_T", causal= TRUE, name = "DoC", autoRun = getOption("umx_auto_run"), intervals = FALSE, tryHard = c("no", "yes", "ordinal", "search"), optimizer = NULL) {
	# TODO: umxDoC add some name checking to avoid variables like "a1"
	tryHard = match.arg(tryHard)
	umx_check(is.logical(causal), "stop", "causal must be TRUE or FALSE")
	if(name == "DoC"){ name = ifelse(TRUE, "DoC", "Chol") }

	nLat1   = length(var1Indicators) # measures for factor 1
	nLat2   = length(var2Indicators)
	nVar    = nLat1 + nLat2
	selVars = tvars(c(var1Indicators, var2Indicators), sep= sep)
	xmu_twin_check(selDVs= c(var1Indicators,var2Indicators), sep = sep, dzData = dzData, mzData = mzData, enforceSep = TRUE, nSib = 2, optimizer = optimizer)
	mzData = xmu_make_mxData(mzData, manifests = selVars)
	dzData = xmu_make_mxData(dzData, manifests = selVars)

	as = paste0("as", 1:nVar) 
	cs = paste0("cs", 1:nVar)
	es = paste0("es", 1:nVar)
	
	paths = c(
		# 1. ✓ Make latent ace, as and l1 &l2
		# ac and e & specifics
		umxPath(v1m0 = c("a1", "a2", "c1", "c2", "e1", "e2")), 
		# TODO: might want to fix ace to 1, AND fix LOADING of e @ 1
		umxPath(v1m0 = c(as, cs, es)),
		# trait latent variables
		umxPath(v0m0 = c("l1", "l2")), # Get all their variance from ace

		# 3. ✓ Load ace paths onto each latent
		umxPath(c("a1", "c1"), to = "l1"),
		umxPath(c("a2", "c2"), to = "l2"),
		umxPath("e1", free = FALSE, values = 1, to = "l1", lbound = 1e-5),
		umxPath("e2", free = FALSE, values = 1, to = "l2", lbound = 1e-5),

		# 4. ✓ Add factor loadings from l1 and l2 onto manifests
		umxPath("l1", to = var1Indicators),
		umxPath("l2", to = var2Indicators),

		# 5. ✓ Load specifics onto var1 and var2 indicators
		umxPath(as, to = c(var1Indicators, var2Indicators), values = .6),
		umxPath(cs, to = c(var1Indicators, var2Indicators), values = .6),
		umxPath(es, to = c(var1Indicators, var2Indicators), values = .6, lbound=1e-5),

		# 7. Generate the causal beta paths
		umxPath("l1", to = "l2", free=FALSE, labels = "a2b", values= 0),
		umxPath("l2", to = "l1", free=FALSE, labels = "b2a", values= 0)
	)

	model = umxTwinMaker(name=name, paths= paths, mzData= mzData, dzData= dzData, 
		sep =sep, t1_t2links = list('a'=c(1, .5), 'c'=c(1, 1), 'e'=c(0, 0), 'as'=c(1, .5), 'cs'=c(1, 1), 'es'=c(0, 0))
	)
	
	return(model)
}

#' Make a twin model from the model describing just one person
#'
#' @description
#' `xmu_path2twin` takes a collection of paths describing the model for 1 person
#' and returns a completed twin model. This consists of a [umxSuperModel()] containing
#' `MZ` and `DZ` [umxRAM()] models.
#'
#' Pass into `umxTwinMaker`:
#'
#' 1. A list of `paths` making up the twin 1 model
#' 2. In `t1_t2links`, a vector describing the component relationships connecting twin 1 to twin 2 
#'  (The default here is 1 and .5 for the a, and, for c and e are 1 and 0 in both groups, respectively.
#'
#' *Details*
#'
#' Some rules. All labels are expanded with a twin suffix: so "var1" -> "var1_T1" etc. so you
#' provide the person-model using just the base name (and tell [umxTwinMaker()] how to expand it by providing a separator string).
#' 
#' Rule 2: The latent a, c, and e latent variables must be labelled to match the base name given in t1_t2links.
#' To avoid clashes, variables must not match the numbered variables in `t1_t2links`  - by default names like "a1" are reserved for ace.
#' 
#' @param name The name for the resulting [umxSuperModel()] (Default "m1")
#' @param paths A vector of [umxPath()]s describing one person
#' @param t1_t2links base name (and values) of paths that covary between T1 and T2. Default: c('a'=c(1,.5), 'c'=c(1,1), 'e'=c(0,0))
#' @param mzData Data for MZ twins
#' @param dzData Data for DZ twins
#' @param sep The separator used to create twin 1 and 2 names (Default "_T")
#' @return - [umxSuperModel()]
#' @export
#' @family Twin Modeling Functions
#' @seealso - [umxRAM()], [umxSuperModel()], [umxPath()]
#' @references - [tutorials](https://tbates.github.io), [github](https://github.com/tbates/umx)
#' @md
#' @examples
#' \dontrun{
#' # We'll make some ACE models, but first, let's clean up the twinData 
#' # set for analysis
#' # 1. Add a separator to the twin variable names (with sep = "_T")
#' # 2. Scale the data so it's easier for the optimizer.
#' data(twinData)
#' tmp = umx_make_twin_data_nice(data=twinData, sep="", zygosity="zygosity", numbering=1:2)
#' tmp = umx_scale_wide_twin_data(varsToScale= c("wt", "ht"), sep= "_T", data= tmp)
#' mzData = subset(tmp, zygosity %in%  c("MZFF", "MZMM"))
#' dzData = subset(tmp, zygosity %in%  c("DZFF", "DZMM"))
#' 
#' # ==========================
#' # = Make an ACE twin model =
#' # ==========================
#' # 1. Define paths for *one* person:
#' paths = c(
#'    umxPath(v1m0 = c("a1", 'c1', "e1")),
#'    umxPath(means = c("wt")),
#'    umxPath(c("a1", 'c1', "e1"), to = "wt", values=.2)
#')
#' # 2. Make a twin model from the paths for one person
#' m1 = umxTwinMaker("test", paths, mzData = mzData, dzData= dzData)
#' plot(m1, std= TRUE, means= FALSE)
#'
#' # 3. comparison with umxACE...
#' m2 = umxACE(selDVs="wt", mzData = mzData, dzData=dzData, sep="_T")
#'
#' # =====================
#' # = Bivariate example =
#' # =====================
#' latents = paste0(rep(c("a", "c", "e"), each = 2), 1:2)
#' biv = c(
#'	umxPath(v1m0 = latents),
#'	umxPath(mean = c("wt", "ht")),
#'	umxPath(fromEach = c("a1", 'c1', "e1"), to = c("ht", "wt")),
#'	umxPath(c("a2", 'c2', "e2"), to = "wt")
#')
#' tmp= umxTwinMaker(paths= biv, mzData = mzData, dzData= dzData)
#' plot(tmp, means=FALSE)
#'
#' # How to use latents other than a, c, and e: define in t1_t2links
#' paths = c(
#'	umxPath(v1m0 = c("as1", 'c1', "e1")),
#'	umxPath(means = c("wt")),
#'	umxPath(c("as1", 'c1', "e1"), to = "wt", values=.2)
#')
#' m1 = umxTwinMaker("test", paths, mzData = mzData, dzData= dzData, 
#' 	t1_t2links = list('as'=c(1, .5), 'c'=c(1, 1), 'e'=c(0, 0))
#' )
#' 
#' }
#'
umxTwinMaker <- function(name = "m1", paths, t1_t2links = list('a'=c(1, .5), 'c'=c(1, 1), 'e'=c(0, 0)), mzData = NULL, dzData= NULL, sep = "_T"){
	# TODO
	# Ensure labels that might need equating of freeing across MZ/DZ or T1 T2
	# Check no manifests match the t1_t2links?
	# Check t1_t2links looks sane (list of 2-number objects)
	# Ensure sep is not ""
	umx_check(sep != "", "stop", "umxTwinData needs a separator like '_T' in the variable names.\nUse umx_make_twin_data_nice() to do this.")

	# Check all paths are paths
	for (i in length(paths)) {
		if(!inherits(paths[[i]], "MxPath")){
			stop("all elements of paths must be a umxPath. Item ", i, "seems to be class ", omxQuotes(class(paths[[i]]) ) )
		}
	}

	# 1. Make twin 1 twin 2 versions of the paths (no labels yet...)
	Twin1Paths = xmu_path2twin(paths = paths, thisTwin = 1, sep = sep)
	Twin2Paths = xmu_path2twin(paths = paths, thisTwin = 2, sep = sep)

	# 2. Make MZ and DZ models with these paths, labelled, and with the right data
	MZ = umxRAM("MZ", c(Twin1Paths, Twin2Paths), data = mzData, autoRun=FALSE)
	DZ = umxRAM("DZ", c(Twin1Paths, Twin2Paths), data = dzData, autoRun=FALSE)

	# At this point, the paths are in the A/S/M matrices BUT T1 and T2 have different labels...
	# TODO: equate free Asymmetric paths running from a latent to a manifest for T1 and T2 
	# example = "a1_T1_to_wt_T1"
	# Where   = MZ$A$labels
	# Might be as simple as delete "_T[1-2]" from all free paths in A matrix
	MZ$A$labels[MZ$A$free] = namez(MZ$A$labels[MZ$A$free], pattern = paste0(sep, "[0-9]+"), replacement= "", global=TRUE)
	DZ$A$labels[DZ$A$free] = namez(DZ$A$labels[DZ$A$free], pattern = paste0(sep, "[0-9]+"), replacement= "", global=TRUE)

	# "one_to_wt_T1" -> "one_to_wt"
	MZ$M$labels[MZ$M$free] = namez(MZ$M$labels[MZ$M$free], pattern = paste0(sep, "[0-9]+$"), replacement= "")
	DZ$M$labels[DZ$M$free] = namez(DZ$M$labels[DZ$M$free], pattern = paste0(sep, "[0-9]+$"), replacement= "")

	# get some name lists that will be handy
	varNames      = dimnames(MZ$S$values)[[1]] # all objects on the diagram
	manifestNames = dimnames(MZ$F)[[1]]
	latentNames   = setdiff(varNames, manifestNames)

	# 3. Add a/c/e twin-covariance path(s)
	for (varComponent in names(t1_t2links)) {
		# varComponent = names(t1_t2links)[1]
		# a. find all instances of this base component, i.e., a1 a2 a3, etc.
		thisSearch = paste0("^", varComponent, "[0-9]+",sep, "1$")
		allPathsOfComponent = namez(varNames, thisSearch)
		# b. Just the component indexes (strip a/c/e (varComponent) and suffix)
		allPathsOfComponent = namez(allPathsOfComponent, pattern = paste0("^", varComponent, "([0-9]+)", sep, "[0-9]$"), replacement= "\\1")
		allPathsOfComponent = as.numeric(allPathsOfComponent)
		# iterate over it
		MZvalue = t1_t2links[[varComponent]][1]
		DZvalue = t1_t2links[[varComponent]][2]
		for (i in allPathsOfComponent) {
			# i = allPathsOfComponent[1]
			T1 = paste0(varComponent, i, sep, "1")
			T2 = paste0(varComponent, i, sep, "2")
			MZ = mxModel(MZ, umxPath(T1, with = T2, free=FALSE, values = MZvalue, labels=paste0(T1, "_MZr_", T2)))
			DZ = mxModel(DZ, umxPath(T1, with = T2, free=FALSE, values = DZvalue, labels=paste0(T1, "_DZr_", T2)))
		}
	}
	model = umxSuperModel(name, MZ, DZ)
	
	# TODO: equate means: "wt_T1_with_wt_T1" "wt_T2_with_wt_T2"
	# 4. Equate means in auto-added means model
	model = as(model, "MxModelTwinMaker") # set class so that S3 plot() dispatches
	return(model)
}


#' Create and display a graphical path diagram for a path-based twin model.
#'
#' Assumes the model has a group called "MZ" inside.
#' 
#' If you use umx_set_plot_format("graphviz"), they will open in a graphviz helper app (if installed).
#' The commercial application \dQuote{OmniGraffle} is great for editing these images.
#' On unix and windows, [plot()] will create a pdf and open it in your default pdf reader.
#' 
#'
#' @aliases umxPlotMxModelTwinMaker
#' @param x A [umxTwinMaker()] model from which to make a path diagram
#' @param std Whether to standardize the model (default = FALSE)
#' @param fixed Whether to show fixed paths (defaults to TRUE)
#' @param means Whether to show means or not (default = TRUE)
#' @param oneTwin (whether to plot a pair of twins, or just one (default = TRUE)
#' @param sep The separator for twin variables ("_T")
#' @param digits The number of decimal places to add to the path coefficients
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param labels Whether to show labels on the paths. "none", "labels", or "both" (parameter + label).
#' @param resid How to show residuals and variances default is "circle". Options are "line" & "none"
#' @param strip_zero Whether to strip the leading "0" and decimal point from parameter estimates (default = FALSE)
#' @param splines Whether to allow lines to curve: defaults to TRUE (nb: some models look better with FALSE)
#' @param min optional list of objects to group at the top of the plot. Default (NULL) chooses automatically.
#' @param same optional list of objects to group at the same rank in the plot. Default (NULL) chooses automatically.
#' @param max optional list of objects to group at the bottom of the plot. Default (NULL) chooses automatically.
#' @param ... Optional parameters
#' @export
#' @seealso - [umx_set_plot_format()], [plot.MxModel()], [umxPlotACE()], [umxPlotCP()], [umxPlotIP()], [umxPlotGxE()]
#' @family Plotting functions
#' @md
#' @examples
#' \dontrun{
#' require(umx)
#' # 
#' # =====================
#' # = Make an ACE model =
#' # =====================
#' # 1. Clean data: Add separator and scale
#' data(twinData)
#' tmp = umx_make_twin_data_nice(data=twinData, sep="", zygosity="zygosity", numbering=1:2)
#' tmp = umx_scale_wide_twin_data(varsToScale= c("wt", "ht"), sep= "_T", data= tmp)
#' mzData = subset(tmp, zygosity %in%  c("MZFF", "MZMM"))
#' dzData = subset(tmp, zygosity %in%  c("DZFF", "DZMM"))
#' 
#' # 2. Define paths: You only need the paths for one person:
#' paths = c(
#'	umxPath(v1m0 = c("a1", 'c1', "e1")),
#'	umxPath(means = c("wt")),
#'	umxPath(c("a1", 'c1', "e1"), to = "wt", values=.2)
#')
#' m1 = umxTwinMaker("test", paths, mzData = mzData, dzData= dzData)
#' plot(m1, std= TRUE, means= FALSE)
#' plot(m1, means=FALSE, std=TRUE, strip=TRUE, splines="FALSE", max="intercept")
#' } # end dontrun
#'
#'# =================
#'# = An ACEv model =
#'# =================
#' # Not complete
#'
#' paths = c(
#'	umxPath(v1m0 = c("A1", 'C1', "E1")),
#'	umxPath(v1m0 = c("A2", 'C2', "E2")),
#'	umxPath(v.m0 = c("l1", 'l2')),
#'	umxPath(v.m. = c("wt", "ht")),
#'	umxPath(c("A1", 'C1', "E1"), to = "l1", values= .2),
#'	umxPath(c("A2", 'C2', "E2"), to = "l2", values= .2),
#'	umxPath(c("l1", 'l2'), to = c("wt", "ht"), values= .2)
#' )
#'
plot.MxModelTwinMaker <- function(x = NA, std = FALSE, fixed = TRUE, means = TRUE, oneTwin = TRUE, sep= "_T", digits = 2, file = "name", labels = c("none", "labels", "both"), resid = c("circle", "line", "none"), strip_zero = FALSE, splines = TRUE, min= NULL, same= NULL, max= NULL, ...) {
	# loop over submodels	
	if(length(x@submodels) && !oneTwin){
		n = 1
		for (sub in x@submodels) {
			if(file == "name"){
				thisFile = file
			} else {
				thisFile = paste0(file, "_group_", n)
			}
			plot.MxModelTwinMaker(sub, std = std, fixed = fixed, means = means, digits = digits, file = file, labels = labels, resid = resid, strip_zero = strip_zero, splines = splines, min= min, same= same, max= max, ...)
			n = n + 1
		}
	}else{	
		# ==========
		# = Setup  =
		# ==========
		model   = x$MZ # just to be clear that x is a model
		resid   = match.arg(resid)
		labels  = match.arg(labels)
	
		# Update values using compute = T to capture labels with [] references.
		# TODO: !!! Needs more work to sync with confidence intervals and SEs
		model$S$values = mxEval(S, model, compute = TRUE)
		model$A$values = mxEval(A, model, compute = TRUE)
		if(!is.null(model$M)){
			model$M$values = mxEval(M, model, compute = TRUE)
		}
	
		if(std){ model = xmu_standardize_RAM(model, return = "model") }

		# ========================
		# = Get Symmetric & Asymmetric Paths =
		# ========================
		out = "";
		# TODO Code to delete rows & columns from A, S & M where dimnames() contain "_T2", i.e., suffix 2
		# And then delete 1 from the dimnames?
		if(oneTwin){
			rowsToKeep = namez(dimnames(model$matrices$A)[[1]], paste0(sep, "1$"))
			# colsToDelete = namez(dimnames(model$matrices$A)[[2]], paste0(sep, "[2-9]$")
			latents = model@latentVars[model@latentVars %in% rowsToKeep]    # 'vis', 'math', and 'text' 
			selDVs  = model@manifestVars[model@manifestVars  %in% rowsToKeep] # 'visual', 'cubes', 'paper', 'general', 'paragrap'...
			A       = model$matrices$A[rowsToKeep, rowsToKeep]
			S       = model$matrices$S[rowsToKeep, rowsToKeep]			
		} else {
			latents = model@latentVars # 'vis', 'math', and 'text' 
			selDVs  = model@manifestVars # 'visual', 'cubes', 'paper', 'general', 'paragrap'...
			A       = model$matrices$A
			S       = model$matrices$S
		}
		
		out = xmu_dot_make_paths(A, stringIn = out, heads = 1, fixed = fixed, labels = labels, comment = "Single arrow paths", digits = digits)
		if(resid == "circle"){
			out = xmu_dot_make_paths(S, stringIn = out, heads = 2, showResiduals = FALSE, fixed = fixed, labels = labels, comment = "Covariances", digits = digits)
		} else if(resid == "line"){
			out = xmu_dot_make_paths(S, stringIn = out, heads = 2, showResiduals = TRUE , fixed = fixed, labels = labels, comment = "Covariances & residuals", digits = digits)
		}else{
			out = xmu_dot_make_paths(S, stringIn = out, heads = 2, showResiduals = FALSE , fixed = fixed, labels = labels, comment = "Covariances & residuals", digits = digits)		
		}
		# TODO should xmu_dot_make_residuals handle fixed or not necessary?
		tmp = xmu_dot_make_residuals(S, latents = latents, digits = digits, resid = resid)
		variances     = tmp$variances     # either "var_var textbox" or "var -> var port circles"
		varianceNames = tmp$varianceNames # names of residuals/variances. EMPTY if using circles 

		# =================
		# = Define shapes =
		# =================
		if(splines){
			preOut = '\tsplines="TRUE";\n\t# Latents\n'
		} else {
			preOut = '\tsplines="FALSE";\n\t# Latents\n'
		}
		for(var in latents) {
		   preOut = paste0(preOut, "\t", var, " [shape = circle];\n")
		}

		preOut = paste0(preOut, "\n\t# Manifests\n")
		for(var in selDVs) {
		   preOut = paste0(preOut, "\t", var, " [shape = square];\n")
		}

		# ================
		# = handle means =
		# ================
		if(umx_has_means(model) & means){
			out = paste0(out, "\n\t# Means paths\n")
			# Add a triangle to the list of shapes
			preOut = paste0(preOut, "\t one [shape = triangle];\n")
			mxMat        = model$matrices$M[,rowsToKeep]
			mxMat_vals   = mxMat$values
			mxMat_free   = mxMat$free
			mxMat_labels = mxMat$labels
			meanVars = colnames(mxMat$values)
			for(to in meanVars) {
				thisPathLabel = mxMat_labels[1, to]
				thisPathFree  = mxMat_free[1, to]
				thisPathVal   = round(mxMat_vals[1, to], digits)
				labelStart    = ifelse(thisPathFree, ' [label="', ' [label="@')

				# TODO plot.MxModel: Find way to show means fixed @0
				if(thisPathFree || fixed ) {
					# if(thisPathFree | (fixed & thisPathVal != 0) ) {
					out = paste0(out, "\tone -> ", to, labelStart, thisPathVal, '"];\n')
				}else{
					# cat(paste0(out, "\tone -> ", to, labelStart, thisPathVal, '"];\n'))
					# return(thisPathVal != 0)
				}
			}
		}

		# ===========================
		# = Make the variance lines =
		# ===========================
		# x1_var [label="0.21", shape = plaintext];
		# or (circles)
		# x1 -> x1 [label="0.21", direction = both];
		preOut = paste0(preOut, "\n\t#Variances/residuals\n")
		for(var in variances) {
		   preOut = paste0(preOut, "\t", var, ";\n")
		}
		# ======================
		# = Set the ranks e.g. =
		# ======================
		# {rank=same; x1 x2 x3 x4 x5 };
		# TODO more intelligence possible in plot() perhaps hints like "MIMIC" or "ACE"
		if(umx_has_means(model)){ append(varianceNames, "one")}

		# min = latents; same = selDVs; max = varianceNames

		# TODO separate [ace]1 (top) from "[ace]s" (bottom)
		x = xmu_dot_move_ranks(max = max, min = min, same=same, old_min = latents, old_same = selDVs, old_max = varianceNames)
		rankVariables = xmu_dot_rank_str(min = x$min, same = x$same, max = x$max)

		# ===================================
		# = Assemble full text to write out =
		# ===================================
		label = model$name
		digraph = paste0(
			"digraph G {\n    ", 
			'label="', label, '";\n',
			preOut, "\n",
			out, "\n",
			rankVariables, "\n}"
		)
		message("\n?plot.MxModel options: std, means, digits, strip_zero, file, splines=T/F, min=, max =, same = , fixed, resid= 'circle|line|none'")
		xmu_dot_maker(model, file, digraph, strip_zero = strip_zero)
	}
} # end plot.MxModelTwinMaker



#' Re-name variables in umxPaths to twin versions
#'
#' @description
#' `xmu_path2twin` takes a collection of paths that use base variable names, 
#' and returns a model with twin names.
#'
#' @details
#' A path like `a to b` will be returned as `a_T1 to b_T1`.
#' @param paths A collection of paths using base variable names.
#' @param thisTwin The twin we are making (i.e., "_T1", or "_T2")
#' @param sep The separator (default "_T")
#' @return - list of relabeled paths
#' @export
#' @family xmu internal not for end user
#' @seealso - [umxTwinMaker()], [umxRAM()]
#' @md
#' @examples
#' twin1PathList = c(
#'	umxPath(v1m0 = c("a1", 'c1', "e1")),
#'	umxPath(fromEach = c("a1", 'c1', "e1"), to = "NFC3", values=.2)
#')
#' xmu_path2twin(twin1PathList, thisTwin = 2)
#'
xmu_path2twin <- function(paths, thisTwin = 1, sep = "_T"){
	# TODO check paths isn't a list
	suffix = paste0(sep, thisTwin)
	for (i in 1:length(paths)){
		thisPath = paths[[i]]
		thisPath@from = xmu_path_regex(thisPath$from, "$", suffix)
		thisPath@to   = xmu_path_regex(thisPath$to  , "$", suffix)
	        if (grepl("^data.", thisPath@labels)) {
      			thisPath@labels = xmu_path_regex(thisPath@labels, "$", paste0(thisPath$label, suffix))
    		}
		paths[[i]] = thisPath
	}
	paths
}

#' Re-name variables umxPaths to twin versions
#'
#' @description
#' `xmu_path2twin` takes a collection of [umxPath()]s (use base variable names), 
#' and returns a model for both twins (and using the expanded variable names).
#'
#' @details
#' A path like `a to b` will be returned as `a_T1 to b_T1`.
#' @param input vector of path labels
#' @param pattern = pattern to match and replace
#' @param replacement = replacement string
#' @param ignore Labels to ignore (reserved words like "one")
#' @return - renamed paths
#' @export
#' @family xmu internal not for end user
#' @seealso - [xmu_path2twin()], [umxTwinMaker()]
#' @references - [tutorials](https://tbates.github.io), [github](https://github.com/tbates/umx)
#' @md
#' @examples
#' xmu_path_regex(c("a", "one", "b"), pattern = "$", replacement = "_T1")
#' # "a_T1" "one"  "b_T1"
xmu_path_regex <- function(input, pattern = NA, replacement = NA, ignore = "one"){
	if(any(is.na(input))){
		return(input)
	}else{
		for (i in 1:length(input)) {
			if(input[i] %in% ignore){
				# nothing to do
			} else {
				input[i] = namez(input[i], pattern = pattern, replacement = replacement)
			}
		}
		return(input)
	}
}
