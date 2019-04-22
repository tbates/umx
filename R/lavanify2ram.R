#' Convert a lavaan syntax string to a umxRAM model
#'
#' @description
#' Use lavaan syntax to create umxRAM models. If data are provided, a `umxRAM` model is returned. 
#' If more than one group is found, a superModel is returned.
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
#'
#' @param lav a lavaan syntax string, e.g. "A~~B"
#' @param data data to add to model (defaults to auto, which is just sketch mode)
#' @param lavaanMode Defaults for automagical paths with lavaan syntax input (default = "sem")
#' @param printTab = TRUE (more for debugging)
#' @param group = NULL TODO: define this
#' @return - list of \code{\link{umxPath}}s
#' @export
#' @family Super-easy helpers
#' @md
#' @seealso - \code{\link{umxRAM}}
#' @examples
#'
#' # auto-data, print table, return umxRAM model
#' m1 = umxLav2RAM("y ~ x")
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
#' # Factor model showing auto-addition of correlations among exogenous latents
#' # and auto-residuals on manifests
#' HS = "spatial =~ visual   + cubes    + flags
#'       verbal  =~ paragrap + sentence + wordm
#'       speed   =~ addition + counting + straight"
#'
#' cov(HS.ability.data[, c("visual"  , "cubes"   , "flags")])
#' cov(HS.ability.data[, c("paragrap", "sentence", "wordm")])
#' cov(HS.ability.data[, c("addition", "counting", "straight")])
#'
#' m1 = umxRAM2(HS, data = HS.ability.data)
#' m1 = umxRAM2(HS, data = umx_scale(HS.ability.data))
#' 
#' tmp = umxLav2RAM("L =~ X1 + X2; L ~ Y")
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
#' lav = '
#'	y ~ b1*x1 + b2*x2 + b3*x3
#'	# constraints
#'	b1 == (b2 + b3)^2
#'	b1 > exp(b2 + b3)'
#'
#' tmp = umxLav2RAM(lav)
#'
umxLav2RAM <- function(lav, data = "auto", lavaanMode = "sem", printTab = TRUE, group = NULL){
	# =~  =  L  -> A
	# ~   =  y <-  x
	# ~~  =  A <-> B
	# <-  =  man -> Latent ((formative factor (+ biv?))
	# lav = ("A ~ B")
	# lav = ("y ~ x1 + 2.4*x2 + x3)
	# lavaanify("y ~ x")
	# TODO accept a list of these properties as lavaan="sem"
	tab = lavaan::lavaanify(model = lav,
		int.ov.free     = TRUE,
		int.lv.free     = FALSE,
		auto.fix.first  = TRUE, # (unless std.lv = TRUE),
		std.lv          = FALSE,
		auto.fix.single = TRUE,
		auto.var        = TRUE,
		auto.cov.lv.x   = TRUE,
		auto.th         = TRUE,
		auto.delta      = TRUE,
		auto.cov.y      = TRUE,
		fixed.x = FALSE # not standard in lavaan::sem, but needed for RAM
	)

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
	message("Found ", nAlg, " algebras :=/group 0 items")
	
	# Remove group 0 from the big table
	tab     = tab[tab$group != 0, ]
	groups  = unique(tab[, "group"])
	nGroups = length(groups)
	message("Found ", nGroups, " group")
	modelList = list()
	for (groupNum in groups) {
		# Process a group/Model
		tmp = xmu_lavaan_process_group(tab, groupNum = groupNum)
		latents   = tmp$latents
		manifests = tmp$manifests
		plist     = tmp$plist
		# All model paths in plist: time to make the RAM model
		# figure out data
		if(!is.null(data)){
			# TODO need to subset data for each group...
			if(is.character(data) && length(data) == 1 && data == "auto"){
				data = manifests
			}
			m1 = umxRAM(paste0("m", groupNum), plist, data = data)
			modelList = append(modelList, m1)
		} else {
			# Could go mxModel here? (allow no data by supplying latents and manifests)
			modelList = append(modelList, plist)
		}
	}

	if(nGroups > 1){
		# Build a super model containing the list of models (groups)
		model = umxSuperModel("top", modelList)
		# Add any algebras to the super?
		tmp = xmu_lavaan_process_group(algebraRows, groupNum = 0)
		model  = mxModel(model, tmp$plist)
	} else {
		# Just one model
		model = modelList[[1]] # (should be just a model)
		# Add algebras	(if any)
		tmp = xmu_lavaan_process_group(algebraRows, groupNum = 0)
		model  = mxModel(model, tmp$plist)
	}
	return(model)
}

#' RAM that can detect lavaan string input
#'
#' @description
#' RAM that can detect lavaan string input is a function which 
#'
#' @param model a lavaan string
#' @param lavaanMode = "sem"
#' @param data optionally provide data
#' @param printTab print the table (defaults to FALSE) # TODO just verbose
#' @return - \code{\link{mxModel}}
#' @export
#' @family Super-easy helpers
#' @seealso - \code{\link{umxLav2RAM}}
#' @examples
#' umxRAM2("y~x") 
#' umxRAM2("y is x") 
#'
umxRAM2 <- function(model, data = NULL, lavaanMode = "sem", printTab = FALSE){
	if (is.character(model) && grepl(model, pattern = "(~|=~|~~|:=)")){
		lavaanString = model
		if(is.null(data)){
			data = "auto"
		}
		model = umxLav2RAM(lav = lavaanString, data = data, lavaanMode = lavaanMode, printTab = printTab)
	}else{
		message("woot: that doesn't look like a lavaan string to me:")
	}
	return(model)
}


# 
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
	handledOps = c("~", "=~", "~~", ":=", constraintOps)
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
				if(rhs == "1"){
					# Intercepts (mean) lhs = variable name rhs == "1"
					new = umxPath(means = lhs, free = free, values = value, labels = label)
				} else {
					new = umxPath(rhs, to = lhs, free = free, values = value, labels = label)
				}
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