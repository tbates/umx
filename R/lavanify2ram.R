# Any goal of which you are capable of each tiny step, you can attain.

#' Convert lavaan string to a umxRAM model
#'
#' @description
#' Takes a lavaan syntax string and creates the matching one or more [umxRAM()] models.
#' 
#' If data are provided, a [umxRAM()] model is returned. 
#' 
#' If more than one group is found, a [umxSuperModel()] is returned.
#' 
#' This function is at the alpha quality stage, and **should be expected to have bugs**.
#' Several features are not yet supported. Let me know if you would like them.
#'
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
#' * fixed.x         = FALSE (not standard in `lavaan::sem`, but needed for RAM)
#'
#' Lavaan is well documented. For quick reference, some common symbols in lavaan strings are:
#'
#' \tabular{rlll}{
#'   \tab "=~"   \tab lhs (Latent) is manifested by rhs\cr
#'   \tab "~"    \tab lhs "is regressed on" (<- ) rhs\cr
#'   \tab "~~"   \tab lhs covaries with rhs\cr
#'   \tab "~ 1"  \tab lhs has mean\cr
#'   \tab ":="   \tab lhs is defined by rhs (see [OpenMx::mxAlgebra()])\cr
#'   \tab "=="   \tab lhs is constrained == to rhs (see [OpenMx::mxConstraint()] )
#' }
#'
#' @section Naming of multiple groups
#' When multiple groups are found the groups are named "name_grouplevel"
#' White space is replaced with "_" and illegal characters are replaced with "x"
#' 
#' @param model A lavaan syntax string, e.g. "A~~B"
#' @param data Data to add to model (defaults to auto, which is just sketch mode)
#' @param lavaanMode Auto-magical path settings for cfa/sem (default) or no-defaults ("lavaan")
#' @param std.lv = FALSE Whether to set var of latents to 1 (default FALSE). nb. Toggles fix first.
#' @param group = Column to use for multi-group (default = NULL)
#' @param group.equal = what to equate across groups. Default (NULL) means no equates. Options that might be implemented (but not yet: c("loadings", "intercepts", "means", "regressions", "residuals", "covariances")
#' @param std Whether to print estimates. Defaults to FALSE ("raw"), TRUE = "std", for no parameter table use NULL.
#' @param comparison Compare the new model to the old (if updating an existing model: default = TRUE)
#' @param type One of "Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS"
#' @param suffix String to append to each label (useful if model will be used in a multi-group model)
#' @param optimizer optionally set the optimizer (default NULL does nothing)
#' @param allContinuousMethod "cumulants" or "marginals". Used in all-continuous WLS data to determine if a means model needed.
#' @param name Model name (can also add name in # commented first line)
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "mxTryHardOrdinal", "mxTryHardWideSearch"
#' @param autoRun Whether to run the model (default), or just to create it and return without running.
#' @param verbose Whether to tell the user what latents and manifests were created etc. (Default = FALSE)
#' @param printTab = TRUE (more for debugging)
#' @return - list of [umxPath()]s
#' @export
#' @family Miscellaneous Utility Functions
#' @seealso - [umxRAM()]
#' @md
#' @examples
#' # auto-data, print table, return umxRAM model
#' m1 = umxLav2RAM("y ~ x", printTab= TRUE)
#' 
#' lav = "y ~ x1 + 2.4*x2 + x3"
#' tmp = umxLav2RAM(lav, data = "auto", printTab= FALSE)
#'
#' # Add labels to parameters, e.g. "x3_loading" as a loading for x3->x1
#' tmp = umxLav2RAM("x1 ~ x3_loading*x3")
#' umx_print(tmp$A$labels)
#' # |   |x1       |x3         |
#' # |:--|:--------|:----------|
#' # |x1 |x1_to_x1 |x3_loading |
#' # |x3 |x1_to_x3 |x3_to_x3   |
#'
#' # Fix values, e.g. x2 -> y fixed at 2.4
#' tmp = umxLav2RAM("y ~ x1 + 2.4*x2; s =~ 0*y11 + 1*y12 + 2*y13 + 3*y14")
#' 
#' tmp = umxLav2RAM("L =~ X1 + X2; L ~ Y")
#' plot(tmp, min=c("L", "Y"))
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
#' m1 = umxRAM(HS, data = umx_scale(HS.ability.data))
#'
#' # Multiple groups
#' m1 = umxRAM(HS, data = umx_scale(HS.ability.data), group = "school")
#'
#' # More complex:
#'
#' lav = " # Moderated mediation
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
#' tmp = umxRAM(lav)
#' # plot showing ability to influence layout with max min same groupings
#' plot(tmp, max = c("cb", "cn", "cngn"), same = "gnt", min= "INT")
#' 
#' # Algebra: e.g. b1^2
#' m1 = umxRAM("x1~b1*x2; B1_sq := b1^2", data = demoOneFactor)
#' 
#' # Model with constraints and labeled parameters
#' lav = "
#'	y ~ b1*x1 + b2*x2 + b3*x3
#'	# constraints
#'	b1 == (b2 + b3)^2
#'	b1 > exp(b2 + b3)"
#'
#' tmp = umxLav2RAM(lav)
#'
#' namedStr = " 	# my name
#' 	y ~x"
#' m1 = umxRAM(namedStr) 
#'
#' # test for removal of bad chars from name
#' lav = " # Model 14 PROCESS Hayes + - '~', ':', and '= moderated mediation
#' gnt ~ a*cb
#' " 
#' m1 = umxRAM(lav) 
#'
#' # Formative factor
#' # lavaanify("f5 <~ z1 + z2 + z3 + z4")
#'
umxLav2RAM <- function(model = NA, data = "auto", group = NULL, group.equal= NULL, name = NA, lavaanMode = c("sem", "lavaan"), std.lv = FALSE, suffix = "", comparison = TRUE, type = c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS"), allContinuousMethod = c("cumulants", "marginals"), autoRun = getOption("umx_auto_run"), tryHard = c("no", "yes", "mxTryHard", "mxTryHardOrdinal", "mxTryHardWideSearch"), verbose = FALSE, optimizer = NULL, std = FALSE, printTab = TRUE){
	# TODO: make groups independent
	# TODO: support group.equal Equality constraints across multiple
	# groups: "loadings", "intercepts", "means", "regressions", "residuals", "covariances"
	# 
	type                = match.arg(type)
	tryHard             = match.arg(tryHard)
	allContinuousMethod = match.arg(allContinuousMethod)
	lavaanMode          = match.arg(lavaanMode)
	
	# =~  =  L  -> A
	# ~   =  y <-  x
	# ~~  =  A <-> B
	# ~-  =  man -> Latent ((formative factor (+ biv?))
	# lav = ("A ~ B")
	# lav = ("y ~ x1 + 2.4*x2 + x3)
	# lavaanify("y ~ x")
	# TODO accept a list of these properties as lavaan="sem"
	# TODO plot color residuals gray; biv blue; one-way green?
	
	# tmp = umxRAM2("e1~~n1; e2~~n2; e2+n2 ~ e1; n2 ~ n1");
	
	# Process lavaanString
	lavaanString = umx_trim(model)
	
	if(is.null(data)){
		data = "auto"
		# TODO could use a list of group names??
		ngroups = 1
		groupLevels = NA # ??
	}else{
		if(!is.null(group)){
			groupLevels = as.character(unique(data[,group]))
			ngroups = length(groupLevels)
		} else {
			ngroups = 1
			groupLevels = NA # ??
		}
	}

	# Use `name` if provided, otherwise look for #name in line 1, else use "m1"
	name = xmu_name_from_lavaan_str(lavaanString = lavaanString, name = name, default = "m1")

	# TODO umxLav2RAM: detect additional legal options (like auto.var) in the ... list and filter into lavaanify call
	if(lavaanMode == "sem"){
		# model = "x1~b1*x2; B1_sq := b1^2"; std.lv=FALSE
		tab = lavaan::lavaanify(model = model, ngroups = ngroups,
			int.ov.free     = TRUE,
			int.lv.free     = FALSE,
			std.lv          = std.lv,
			auto.fix.first  = !std.lv, # (so will default to TRUE)
			auto.fix.single = TRUE,
			auto.var        = TRUE,
			auto.cov.lv.x   = TRUE, 
			auto.th         = TRUE,
			auto.delta      = TRUE,
			auto.cov.y      = TRUE, 
			group.equal     = group.equal,
			fixed.x         = FALSE # Not standard in lavaan::sem, but needed for RAM
			# If TRUE, would fix mean, var, cov, of exogenous covariates to their sample values.			
		)
	}else	if(lavaanMode == "lavaan"){
		tab = lavaan::lavaanify(model = model, ngroups = ngroups, group.equal = group.equal, std.lv = std.lv, auto.fix.first = !std.lv, fixed.x = FALSE)
	}else{
		message("Only sem and lavaan (only what the user explicitly requests) are implemented as yet: What other modes would be useful?")		
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

	# Pull out group 0 (algebras) if found : might need to create in supergroup.
	algebraRows = tab[tab$group == 0, ]
	nAlg = nrow(algebraRows)

	# Already have
	# groupLevels = as.character(unique(data[,group]))
	# ngroups = length(groupLevels)

	# Remove group 0 from the big table
	tab     = tab[tab$group != 0, ]
	tabGroups  = unique(tab[, "group"]) # numeric
	if(ngroups != length(tabGroups)){
		message("I found ", ngroups, " in the data column ", omxQuotes(group), " but lavaanify found", length(tabGroups))
	}

	# if(ngroups){ message("Found ", ngroups, " groups") }
	# if(nAlg)   { message("Found ", nAlg   , " algebras (:=) or group-0 items")}

	if(is.null(data)){
		sketchMode = TRUE
	} else if( is.character(data) && length(data) == 1 && data == "auto"){
		# auto
		sketchMode = TRUE
	}else{
		sketchMode = FALSE
	}

	modelList = list()
	for (groupNum in tabGroups) {
		# Process a group/Model
		tmp = xmu_lavaan_process_group(tab, groupNum = groupNum)
		latents   = tmp$latents
		manifests = tmp$manifests
		plist     = tmp$plist
		# All model paths in plist: time to make the RAM model

		# Figure out data
		if(sketchMode){
			theseData = manifests
		}else if (ngroups > 1){
			filter = data[, group] == groupLevels[groupNum]
			theseData = data[filter, ]
		}else{
			theseData = data
		}
		if(ngroups > 1){
			modelName = paste0(name, "_", groupLevels[groupNum])
			modelName = gsub("(\\h+)", "_", modelName, perl = TRUE)
			# Delete illegal characters
			modelName = as.character(mxMakeNames(modelName))			
		}else{
			modelName = name
		}
		m1 = umxRAM(modelName, plist, data = theseData, autoRun = FALSE, type = type, allContinuousMethod = allContinuousMethod)
		modelList = append(modelList, m1)
	}

	# ModelList contains a list of models (groups)
	if(ngroups > 1){
		# If more than one, make a superModel
		model = umxSuperModel("top", modelList, autoRun = FALSE)
	} else {
		# Just one model
		model = modelList[[1]] # (should be just a model)
	}
	# Add algebras	(if any)
	tmp   = xmu_lavaan_process_group(algebraRows, groupNum = 0)
	model = mxModel(model, tmp$plist)

	if (class(data) == "character"){
		# User is just running a trial model, with no data, but provided names for sketch mode
		autoPlot = umx_set_auto_plot(silent = TRUE)
		if(autoRun && autoPlot){
			plot(model)
		}
		return(model)
	}else{
		model = omxAssignFirstParameters(model)
		model = xmu_safe_run_summary(model, autoRun = autoRun, tryHard = tryHard, std= std)
		return(model)
	}
}

#' Process table of paths to model
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
#' @seealso - [umxLav2RAM()]
#' @md
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
	if(nrow(grpRows) == 0){
		return(list(plist = list(), latents = c(), manifests = c()))
	}
	latents = unique(grpRows$lhs[grpRows$op == "=~" | grpRows$op == "<~"])
	all = unique(c(grpRows$lhs[! (grpRows$op %in% constraintOps)], grpRows$rhs[! (grpRows$op %in% constraintOps)]))
	manifests = setdiff(all, latents)
	manifests = setdiff(manifests, '') # Operations with no rhs can generate empty labels...

	# Process rows
	plist = list()
	for (r in 1:nrow(grpRows)) {
		# r= 1
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
				stop("Haven't implemented op = ", omxQuotes(op), " yet. email maintainer('umx')")
			}
		}
		# path processed
	}
	return(list(plist= plist, latents = latents, manifests = manifests))
}