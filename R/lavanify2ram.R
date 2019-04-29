#' Make RAM model using lavaan syntax
#'
#' @description
#' Can detect lavaan string input. TODO document once merged with [umxRAM()].
#'
#' @param model A lavaan string
#' @param data Data for the model (optional)
#' @param lavaanMode = "sem"
#' @param std.lv = Whether to set var of latents to 1 (default = FALSE) n.b. Toggles fix.first
#' @param group = Column to use for multi-group (default = NULL)
#' @param autoRun Whether to run the model (default), or just to create it and return without running.
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "mxTryHardOrdinal", "mxTryHardWideSearch"
#' @param printTab Print the table (defaults to FALSE) # TODO just verbose
#' @param name Name for model (optional)
#' @return - [mxModel()]
#' @export
#' @family Super-easy helpers
#' @seealso - [umxLav2RAM()]
#' @examples
#' m1 = umxRAM2("y~x")
#' umxRAM2("y is x") # not a lavaan string
#' namedStr = " 	# my name
#' 	y ~x"
#' m1 = umxRAM2(namedStr) 
#'
#' # test for removal of bad chars from name
#' lav = " # Model 14 PROCESS Hayes + - '~', ':', and '= moderated mediation
#' gnt ~ a*cb
#' " 
#' m1 = umxRAM2(lav) 
#'
umxRAM2 <- function(model, data = NULL, group = NULL, std.lv = FALSE, name = NULL, lavaanMode = "sem", autoRun = TRUE, tryHard = c("no", "yes", "mxTryHard", "mxTryHardOrdinal", "mxTryHardWideSearch"), printTab = FALSE){
	if (is.character(model) && grepl(model, pattern = "(<|~|=~|~~|:=)")){
		# Process lavaanString
		lavaanString = umx_trim(model)

		if(!is.null(group)){
			stop("Support for group = not implemented yet. coming shortly")
		}
		
		# Assume set name is the one the user wants if !is.null(name)
		if(is.null(name)){
			# If first line contains a #, assume user wants it to be a name for the model
			line1 = strsplit(lavaanString, split="\\n", perl = TRUE)[[1]][1]
			if(grepl(x= line1, pattern= "#")){
				# line1 = "## my model ##"
				pat = "\\h*#+\\h*([^\\n#]+).*" # remove leading #, trim
				name = gsub(x= line1, pattern= pat, replacement= "\\1", perl= TRUE);
				name = trimws(name)
				# Replace white space with  "_"
				name = gsub("(\\h+)", "_", name, perl=TRUE)
				# Delete illegal characters
				name = as.character(mxMakeNames(name))
			}else{
				# No name given in name or comment: use a default name
				name = "m1"
			}
		}

		if(is.null(data)){
			data = "auto"
		}
		model = umxLav2RAM(model = lavaanString, data = data, group = group, std.lv = std.lv, name = name, lavaanMode = lavaanMode, autoRun = autoRun, tryHard = tryHard, printTab = printTab)
		# umxLav2RAM will run the subModels with umxRAM as they were built.
		invisible(model)
	}else{
		message("Woot: that doesn't look like a lavaan string to me:")
	}
	return(model)
}

#' Convert a lavaan syntax string to a umxRAM model
#'
#' @description
#' Takes a lavaan syntax string and creates the matching one or more umxRAM models.
#' 
#' If data are provided, a [umxRAM()] model is returned. 
#' 
#' If more than one group is found, a [umxSuperModel()] is returned.
#' 
#' This function is at the alpha quality stage, and **should be expected to have bugs**.
#' Several features are not yet supported. Let me know if you'd like them.
#' @details
#'
#' Uses the defaults of `lavaan::sem`
#' 
#' * int.ov.free     = TRUE
#' * int.lv.free     = FALSE
#' * auto.fix.first  = TRUE (unless std.lv = TRUE)
#' * auto.fix.single = TRUE
#' * auto.var        = TRUE
#' * auto.cov.lv.x   = TRUE
#' * auto.th         = TRUE
#' * auto.delta      = TRUE
#' * auto.cov.y      = TRUE
#' * fixed.x         = FALSE (not standard in lavaan::sem, but needed for RAM)
#'
#' Lavaan is fabulously well documented. For quick reference, some common symbols in lavaan strings are
#' 
#'
#' \tabular{rlll}{
#'   \tab "=~"   \tab lhs (Latent) is manifested by rhs\cr
#'   \tab "~"    \tab lhs "is regressed on" (<- ) rhs\cr
#'   \tab "~~"   \tab lhs covarys with rhs\cr
#'   \tab "~ 1"  \tab lhs has mean\cr
#'   \tab ":="   \tab lhs is defined by rhs (see [OpenMx::mxAlgebra()])\cr
#'   \tab "=="   \tab lhs is constrained == to rhs (see [OpenMx::mxConstraint()] )
#' }
#'
#' 
#' @param model A lavaan syntax string, e.g. "A~~B"
#' @param data Data to add to model (defaults to auto, which is just sketch mode)
#' @param lavaanMode Automagical path settings for cfa or sem (default)
#' @param group = Column to use for multi-group (default = NULL)
#' @param name Model name (can also add name in # commented line-1)
#' @param std.lv = FALSE Whether to set var of latents to 1 (default FALSE). nb. Toggles fix first.
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "mxTryHardOrdinal", "mxTryHardWideSearch"
#' @param autoRun Whether to run the model (default), or just to create it and return without running.
#' @param printTab = TRUE (more for debugging)
#' @return - list of [umxPath()]s
#' @export
#' @family Super-easy helpers
#' @md
#' @seealso - [umxRAM()]
#' @examples
#' # auto-data, print table, return umxRAM model
#' m1 = umxLav2RAM("y ~ x", printTab= FALSE)
#' 
#' lav = "y ~ x1 + 2.4*x2 + x3"
#' tmp = umxLav2RAM(lav, data = "auto", printTab= FALSE)
#'
#' # Add labels to parameters, e.g. "x3_loading" as a loading for x3->x1
#' tmp = umxLav2RAM("x1 ~ x3_loading*x3")
#'
#' # Fix values, e.g. x2 -> y fixed at 2.4
#' tmp = umxLav2RAM("y ~ x1 + 2.4*x2; s =~ 0*y11 + 1*y12 + 2*y13 + 3*y14")
#' 
#' tmp = umxLav2RAM("L =~ X1 + X2; L ~ Y")
#' 
#' # Factor model showing auto-addition of correlations among exogenous latents
#' # and auto-residuals on manifests
#' data("HS.ability.data", package = "OpenMx")
#'
#' cov(HS.ability.data[, c("visual"  , "cubes"   , "flags")])
#' cov(HS.ability.data[, c("paragrap", "sentence", "wordm")])
#' cov(HS.ability.data[, c("addition", "counting", "straight")])
#'
#' HS = "spatial =~ visual   + cubes    + flags
#'       verbal  =~ paragrap + sentence + wordm
#'       speed   =~ addition + counting + straight"
#' 
#' m1 = umxRAM2(HS, data = umx_scale(HS.ability.data))
#'
#' # more complex:
#'
#' lav = " # Model 14 PROCESS Hayes - moderated mediation
#' gnt ~ a*cb
#' INT ~ b1*gnt + b2*cn + b3*cngn + c*cb
#' 
#' indirect := a*b1
#' direct := c
#' 
#' ab3 := a * b3
#' loCN := a * b1 + ab3 * -0.5
#' hiCN := a * b1 + ab3 * 0.5
#' "
#' tmp = umxRAM2(lav)
#' # plot showing ability to influence layout with max min same groupings
#' plot(tmp, max = c("cb", "cn", "cngn"), same = "gnt", min="INT")
#' 
#' # Algebra: e.g. b1^2
#' m1 = umxRAM2("x1~b1*x2; B1_sq := b1^2", data = demoOneFactor)
#' 
#' # Model with labeled parameters
#' lav = "
#'	y ~ b1*x1 + b2*x2 + b3*x3
#'	# constraints
#'	b1 == (b2 + b3)^2
#'	b1 > exp(b2 + b3)"
#'
#' tmp = umxLav2RAM(lav)
#'
umxLav2RAM <- function(model = NA, data = "auto", group = NULL, name = NULL, lavaanMode = "sem", std.lv = FALSE, autoRun = TRUE, tryHard = c("no", "yes", "mxTryHard", "mxTryHardOrdinal", "mxTryHardWideSearch"), printTab = TRUE){
	# =~  =  L  -> A
	# ~   =  y <-  x
	# ~~  =  A <-> B
	# ~-  =  man -> Latent ((formative factor (+ biv?))
	# lav = ("A ~ B")
	# lav = ("y ~ x1 + 2.4*x2 + x3)
	# lavaanify("y ~ x")
	# TODO accept a list of these properties as lavaan="sem"
	# TODO color residuals gray; biv blue; one-way green?
	
	# tmp = umxRAM2("e1~~n1; e2~~n2; e2+n2 ~ e1; n2 ~ n1");
	if(is.null(name)){
		name = "m1"
	}
	if(!is.null(group)){
		stop("Support for group = not implemented yet. coming shortly")
	}


	if(lavaanMode == "sem"){
		# model = "x1~b1*x2; B1_sq := b1^2"; std.lv=FALSE
		tab = lavaan::lavaanify(model = model,
			int.ov.free     = TRUE,
			int.lv.free     = FALSE,
			auto.fix.first  = !std.lv, # (so will default to TRUE)
			std.lv          = std.lv,
			auto.fix.single = TRUE,
			auto.var        = TRUE,
			auto.cov.lv.x   = TRUE,
			auto.th         = TRUE,
			auto.delta      = TRUE,
			auto.cov.y      = TRUE,
			fixed.x = FALSE # not standard in lavaan::sem, but needed for RAM
		)
	}else{
		# TODO umxLav2RAM: detect legal options (like auto.var) in the ... list and filter into lavaanify call
		message("Only sem mode implemented as yet: what other modes would be useful?")		
	}

	if(printTab){
		umx_print(tab)
	}

	# tab = lavaanify(model = lav, meanstructure = FALSE, 
	#     orthogonal = FALSE,
	#     conditional.x = FALSE, fixed.x = TRUE, parameterization = "delta",
	#     constraints = NULL, auto = FALSE, model.type = "sem",
	#     varTable = NULL, ngroups = 1L, group.equal = NULL,
	#     group.partial = NULL, group.w.free = FALSE,
	#     debug = FALSE, warn = TRUE, as.data.frame. = TRUE)

	# auto.var=TRUE, auto.fix.first=TRUE, auto.cov.lv.x=TRUE
	#   id lhs op rhs user block group free ustart exo label plabel
	# 1  1   A  ~   B    1     1     1    1     NA   0         .p1.
	# 2  2   A ~~   A    0     1     1    0      0   0         .p2.
	# 3  3   B ~~   B    0     1     1    0     NA   1         .p3.

	# Any goal of which you are capable of each tiny step, you can attain.
	# Pull out group 0 if found (algebras): might need to create in supergroup.
	algebraRows = tab[tab$group == 0, ]
	nAlg = nrow(algebraRows)

	# Remove group 0 from the big table
	tab     = tab[tab$group != 0, ]
	groups  = unique(tab[, "group"])
	nGroups = length(groups)

	# TODO umxLav2RAM: remove this reporting
	if(nGroups){ message("Found ", nGroups, " groups") }
	if(nAlg){ message("Found ", nAlg, " algebras (:=) or group-0 items")}

	modelList = list()
	for (groupNum in groups) {
		# Process a group/Model
		tmp = xmu_lavaan_process_group(tab, groupNum = groupNum)
		latents   = tmp$latents
		manifests = tmp$manifests
		plist     = tmp$plist
		# All model paths in plist: time to make the RAM model

		# figure out data
		# TODO need to subset data for each group...
		if(is.null(data)){
			data = manifests
		} else {
			if(is.character(data) && length(data) == 1 && data == "auto"){
				data = manifests
			}
		}
		if(nGroups > 1){
			modelName = paste0(name, groupNum)
		}else{
			modelName = name
		}
		m1 = umxRAM(modelName, plist, data = data, autoRun = FALSE)
		modelList = append(modelList, m1)
	}

	# ModelList contains a list of models (groups)
	if(nGroups > 1){
		# If more than one, make a superModel
		model = umxSuperModel("top", modelList, autoRun = FALSE)
	} else {
		# Just one model
		model = modelList[[1]] # (should be just a model)
	}
	# Add algebras	(if any)
	tmp   = xmu_lavaan_process_group(algebraRows, groupNum = 0)
	model = mxModel(model, tmp$plist)

	# 2019-04-28: Model not yet run, but umxLav2RAM does run so...
	# I initially ran the subModels with umxRAM as they were built.
	if (class(data) == "character"){
		# User is just running a trial model, with no data, but provided names for sketch mode
		autoPlot = umx_set_auto_plot(silent = TRUE)
		if(autoRun && autoPlot){
			plot(model)
		}
		return(model)
	}else{
		model = omxAssignFirstParameters(model)
		model = xmu_safe_run_summary(model, autoRun = autoRun, tryHard = tryHard)
		return(model)
	}
}


#' lavaan parameter table rows to model
#'
#' @description
#' Process a set of lavaan tables rows forming a group (Model).
#' Returns empty arrays if no rows matching the requested group are found.
#' 
#' @param tab a parameter table
#' @param groupNum group number to filter table on
#' @return - list(plist=plist, latents = latents, manifests = manifests)
#' @export
#' @family xmu internal not for end user
#' @seealso - \code{\link{umxLav2RAM}}
#' @examples
#' tab = lavaan::lavaanify("y~x")
#' xmu_lavaan_process_group(tab, groupNum = 1)
#' xmu_lavaan_process_group(tab, groupNum = 0)
xmu_lavaan_process_group <- function(tab, groupNum){
	constraintOps = c("==", "<", ">")
	handledOps = c("=~", "~", "~1", "~~", ":=", constraintOps)
	# groupNum = 1
	grpRows = tab[tab$group == groupNum, ]
	# handle none exist
	if(nrow(grpRows)==0){
		return(list(plist= list(), latents = c(), manifests = c()))
	}
	latents = unique(grpRows$lhs[grpRows$op == "=~" | grpRows$op == "<~"])
	all = unique(c(grpRows$lhs[! (grpRows$op %in% constraintOps)], grpRows$rhs[! (grpRows$op %in% constraintOps)]))
	manifests = setdiff(all, latents)

	plist = list()
	for (r in 1:nrow(grpRows)) {
		# r= 1
		# umx_msg(r)
		thisRow = grpRows[r, ]

		lhs   = thisRow$lhs
		op    = thisRow$op
		rhs   = thisRow$rhs
		free  = thisRow$free > 0 # in lavaan, free is a list of numbers: same for equated; 0 for free, distinct for unique
		value = thisRow$ustart # often NA
		label = thisRow$label  # likely ""
		label = xmu_clean_label(label, replace = "_")			
		if(label == '') {label = NA}

		if(op %in% handledOps){
			if(op == "~"){
				# Regressions, x on y, 1-headed arrow, rhs = from
					new = umxPath(rhs, to = lhs, free = free, values = value, labels = label)
			} else if(op == "~1"){
				# Intercepts (mean) lhs = variable name rhs == "1"
				new = umxPath(means = lhs, free = free, values = value, labels = label)
			} else if(op == "=~"){
				# Latent variable definitions: "=~", 1-headed arrow, lhs is latent
				new = umxPath(lhs, to = rhs, free = free, values = value, labels = label)
			} else if(op == "~~"){
				# Variance/covariance: "~~", 2-headed arrow
				new = umxPath(lhs, with = rhs, free = free, values = value, labels = label)
			}else if(op==":="){
				# OpenMx Algebra (lavaan calls this "defined as", op ":="
				new = mxAlgebraFromString(name=lhs, algString = rhs)
			}else if(op %in% constraintOps){
				# constraint
				new = mxConstraintFromString(paste(lhs,op, rhs))
			}
			plist = append(plist, new)
		} else {
			if(op=="|"){
				message("Thresholds ('|') not yet implemented for lavaan-2-umx translation")
			}else if(op=="~*~"){
				message("Scaling factors ('~*~' for Delta parameterized multi-group models with categorical indicators) not yet implemented for lavaan-2-umx translation")
			}else{
				# e.g. formative op = "<-" latent on rhs
				stop("Haven't implemented op = ", omxQuotes(op), " yet. tell Tim")
			}
		}
		# path processed
	}
	return(list(plist=plist, latents = latents, manifests = manifests))
}