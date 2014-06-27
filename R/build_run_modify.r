# devtools::document("~/bin/umx")     ; devtools::install("~/bin/umx");
# devtools::check("~/bin/umx")
# devtools::check_doc("~/bin/umx")
# setwd("~/bin/umx"); 
# system(paste("open", shQuote("/Users/tim/bin/umx/R/misc_and_utility.r")))
# devtools::build("~/bin/umx")
# devtools::load_all("~/bin/umx")
# devtools::show_news("~/bin/umx")
# devtools::run_examples("~/bin/umx")

# devtools::create()
# devtools::add_travis();
# devtools::update_version();
# devtools::news();
# devtools::create_README()

# devtools::document("~/bin/umx.twin"); devtools::install("~/bin/umx.twin"); 

# https://r-forge.r-project.org/project/admin/?group_id=1745
# http://adv-r.had.co.nz/Philosophy.html
# https://github.com/hadley/devtools

# ===============================
# = Highlevel models (ACE, GxE) =
# ===============================

#' umxRAM
#'
#' Making it as simple as possible to create a RAM model, without doing things invisible to the user.
#' 
#' umxRAM borrows its philosophy and some parameter labels from the John Fox sem::specifyModel function.
#'
#' @details Like mxModel, you list the theoretical causal paths. Unlike mxModel:
#' \enumerate{
#' \item{You can request creation of error variances using \code{endog.variances = TRUE} }
#' \item{You can request creation of variances for exogenous variables (using \code{exog.variances = TRUE})}
#' \item{For identification, you can request either \code{fix = "latents"} or \code{fix = "firstLoadings"} to fix either the variance of latents or their first factor loading at 1.}
#' }
#' Additional conveniences: 
#' \enumerate{
#' \item{type defaults to "RAM"}
#' \item{You don't need to list manifestVars (they are assumed to map onto names in the \code{mxData})}
#' \item{Any variables you mention that are not found in mxData are assumed to be latents}
#' }
#' 
#' @param name friendly name for the model
#' @param ... A list of mxPaths or mxThreshold objects
#' @param exog.variances If TRUE, free variance parameters are added for exogenous variables that lack them (the default is FALSE).
#' @param endog.variances If TRUE, free error-variance parameters are added for any endogenous variables that lack them (default is FALSE).
#' @param fix Whether to fix latent or first paths to 1. Options are: c("none", "latents", "firstLoadings") (defaults to "none")
#' @param latentVars Latents you want in your model (defaults to NULL, in which case any variable not in the data is assumed to be a latent variable)
#' @param data the data for the model. Can be an \code{\link{mxData}} or a data.frame
#' @param remove_unused_manifests Whether to remove variables in the data to which no path makes reference (defaults to TRUE)
#' @param independent Whether the model is independent (default = NA)
#' @return - \code{\link{mxModel}}
#' @export
#' @family umx model building functions
#' @references - \url{https://github.com/tbates/umx}, \url{tbates.github.io}
#' @examples
#' thedata = mtcars[,c("mpg", "cyl", "disp")]
#' a = mxPath(from = "l_power", to = "mpg")
#' b = mxPath(from = c("cyl","disp"), to = "l_power")
#' d = mxData(thedata, type = "raw")
#' e = mxData(cov(thedata), type = "cov", numObs = nrow(thedata))
#'
#' m1 = umxRAM("tim", a, b, d)
#' m1 = umxRAM("tim", a, b, e)
#' m1 = mxRun(m1)
#' \dontrun{
#' plot(m1)
#' m1 = umxRAM("tim", a, b) # error: no data
#' # TODO implement handling a dataframe
#' umxRAM("tim", a, b, thedata)
#' }
umxRAM <- function(name, ..., exog.variances = FALSE, endog.variances = FALSE, fix = c("none", "latents", "firstLoadings"), latentVars = NULL, data = NULL, independent = NA, remove_unused_manifests = TRUE) {
	fix = umx_default_option(fix, c("none", "latents", "firstLoadings"), check = TRUE)
	dot.items = list(...) # grab all the dot items: mxPaths, etc...
	if(!length(dot.items) > 0){
		stop("You must include some mxPath()s")
	}

	nPaths       = 0 # initialise
	foundNames   = c()
	manifestVars = NULL
	for (i in dot.items) {
		thisIs = class(i)[1]
		if(thisIs == "MxPath"){
			foundNames = append(foundNames, c(i@from, i@to))
		} else {
			if(class(i[[1]])[1] == "MxThreshold"){
				# MxThreshold detected
			}else{
				# stop("I can only handle mxPaths and mxThreshold() objects.\n",
				# "You have given me a", class(i)[1],"\n",
				# " To include data in umxRAM, say 'data = yourData'")
			}
		}
	}

	# ========================
	# = All items processed  =
	# ========================
	# ===============
	# = Handle data =
	# ===============
	if(is.null(data)){
		stop("You must include data: either data = dataframe or data = mxData(yourData, type = 'raw|cov)', ...)")
	} else if(class(data)[1] == "data.frame") {
		data = mxData(observed = data, type = "raw")
	}

    if(class(data)[1] %in%  c("MxNonNullData", "MxDataStatic") ) {
		if(data@type == "raw"){
			manifestVars = names(data@observed)
			isRaw = TRUE
		} else {
			isRaw = FALSE
			manifestVars = colnames(data@observed)
		}
		if(is.null(manifestVars)){
			stop("There's something wrong with the mxData - I couldn't get the variable names from it. Did you set type correctly?")
		}
	} else {
		stop("There's something wrong with the data - I expected a dataframe or mxData, but you gave me a ", class(data)[1])		
	}

	foundNames = unique(na.omit(foundNames))

	if(!is.null(latentVars)){
		nLatent = length(latentVars)
		message("You specified ", nLatent, " latent variables.")
		latentsMentioned = setdiff(foundNames, manifestVars)
		if(any(!(latentVars %in% latentsMentioned))){
			stop(paste0("You requested the following latents, but never mention them in your path list: ", 
				paste(latentVars[!(latentVars %in% latentsMentioned)], collapse = ", "))
			)
		} else if (any(!latentsMentioned %in% latentVars)){
			stop(paste0("You defined some latents, but then use the following (additional) latents in path statements: ", 
					paste(latentsMentioned[!latentsMentioned %in% latentVars], collapse = ", "),"\n",
					"If you want to create latents on the fly, don't specify a defined list."
				)
			)
		}
	} else {
		# Anything not in data -> latent
		latentVars = setdiff(foundNames, c(manifestVars, "one"))
		nLatent = length(latentVars)
		# Report on which latents were created
		if(nLatent == 0){
			message("No latent variables were created.\n")
			latentVars = NA
		} else if (nLatent == 1){
			message("A latent variable '", latentVars[1], "' was created.\n")
		} else {
			message(nLatent, " latent variables were created:", paste(latentVars, collapse = ", "), ".\n")
		}
	}
	# TODO handle when the user adds mxThreshold object: this will be a model where things are not in the data and are not latent...
	# ====================
	# = Handle Manifests =
	# ====================
	unusedManifests = setdiff(manifestVars, foundNames)
	if(length(unusedManifests) > 0){
		if(length(unusedManifests) > 10){
			varList = paste0("The first 10 were: ", paste(unusedManifests[1:10], collapse = ", "), "\n")
		} else {
			varList = paste0("They were: ", paste(unusedManifests, collapse = ", "), "\n")
		}
		message("There were ", length(unusedManifests), " variables in the dataset which were not referenced in any path\n",varList)
		if(remove_unused_manifests){
			# trim down the data to include only the used manifests
			manifestVars = setdiff(manifestVars, unusedManifests)
			if(data@type == "raw"){
				data@observed = data@observed[, manifestVars]
			} else {
				data@observed = umx_reorder(data@observed, manifestVars)
			}
			message("These were dropped from the dataset")
		} else {
			message("I left them in the data. To remove them automatically, next time set remove_unused_manifests = TRUE")
		}		
	}
	message("ManifestVars set to: ", paste(manifestVars, collapse = ", "), "\n")

	m1 = do.call("mxModel", list(name = name, type = "RAM", 
		manifestVars = manifestVars,
		latentVars  = latentVars,
		independent = T,
		data, dot.items)
	)
	# TODO: Add variance/residuals to all variables except reflective latents
	# mxPath(from = fixed, arrows = 2),
	# message("Created model ", m1$name)
	
	# exog == no incoming single arrow paths
	pathList = umx_is_exogenous(m1, manifests_only = TRUE)
	if(exog.variances & length(pathList) > 0 ){
		m1 = umx_add_variances(m1, pathList)
		message("Added variances to ", length(pathList), " exogenous variables: ", paste(pathList, collapse = ", "), "\n")
	}
	
	# endog == one or more incoming single arrow paths
	if(endog.variances){
		pathList = umx_is_endogenous(m1, manifests_only = TRUE)
		if(length(pathList > 0)){
			m1 = umx_add_variances(m1, pathList)
			message("Added variances to ", length(pathList), " endogenous variables: ", paste(pathList, collapse = ", "), "\n")
		} else {
			# message("No endogenous variables found.\n")
		}
	} else{
		# message("endogenous variances not added")
	}

	if(!fix == "none"){
		stop("fix is not supported any longer: switch to umxPath with firstAt and fixedAt to be more up front about model content")
		# TODO turn this off, now that umxPath makes it easy...
		# Fix latents or first paths
		if(fix == "latents"){
			m1 = umx_fix_latents(m1)
		} else if(fix == "firstLoadings"){
			# add free variance to latents not in the fixed list?
			m1 = umx_fix_first_loadings(m1)
		}
	}

	if(isRaw){
		# TODO add means if no means added...
		if(is.null(m1@matrices$M) ){
			message("You have raw data, but no means model. I added\n",
			"mxPath('one', to = manifestVars)")
			m1 = mxModel(m1, mxPath("one", manifestVars))
		} else {
			# leave the user's means as the model
			# print("using your means model")
			# umx_show(m1)
			# print(m1@matrices$M@values)
		}
	}
	m1 = umxLabel(m1)
	m1 = umxValues(m1, onlyTouchZeros = TRUE)
	return(m1)
}

#' umxGxE_window
#'
#' Makes a model to do a GxE analysis using Local SEM (Hildebrandt, Wilhelm & Robitzsch, 2009, p96)
#' Local SEM GxE relies on weighting the moderator to allow conducting repeated regular
#' ACE analyses targeted at sucessive regions of the moderator.
#' In this sense, you can think of it as nonparametric GxE
#' 
#' @param selDVs The dependent variables for T1 and T2, e.g. c("bmi_T1", "bmi_T2")
#' @param moderator The name of the moderator variable in the dataset e.g. "age", "SES" etc.
#' @param mzData Dataframe containing the DV and moderator for MZ twins
#' @param dzData Dataframe containing the DV and moderator for DZ twins
#' @param weightCov Whether to use cov.wt matrices or FIML default = FIML (FALSE)
#' @param width An option to widen or narrow the window from its default (of 1)
#' @param specifiedTargets a user-selected list of moderator values to test (default = NULL = explore the full range)
#' @param plotWindow whether to plot what the window looks like
#' @param return  whether to return the last model (useful for specifiedTargets) or the list of estimates (default = "estimates")
	#' @return - Table of estimates of ACE along the moderator
#' @export
#' @examples
#' library(OpenMx);
#' # ==============================
#' # = 1. Open and clean the data =
#' # ==============================
#' # umxGxE_window takes a dataframe consisting of a moderator and two DV columns: one for each twin
#' moderator = "age"         # The name of the moderator column in the dataset
#' selDVs = c("bmi1","bmi2") # The DV for twin 1 and twin 2
#' data(twinData) # Dataset of Australian twins, built into OpenMx! Use help(twinData) for more
#' # The twinData consist of two cohorts. First we label them
#' # TODO: Q for openmx team: can I add a cohort column to this dataset?
#' twinData$cohort = 1; twinData$cohort[twinData$zyg %in% 6:10] = 2
#' twinData$zyg[twinData$cohort == 2] = twinData$zyg[twinData$cohort == 2]-5
#' # And set a plain-English label
#' twinData$ZYG = factor(twinData$zyg, levels = 1:5, labels = c("MZFF", "MZMM", "DZFF", "DZMM", "DZOS"))
#' # The model also assumes two groups: MZ and DZ. Moderator can't be missing
#' # Delete missing moderator rows
#' twinData = twinData[!is.na(twinData[moderator]),]
#' mzData = subset(twinData, ZYG == "MZFF", c(selDVs, moderator))
#' dzData = subset(twinData, ZYG == "DZFF", c(selDVs, moderator))
#' 
#' # ======================
#' # = Run the analyses! =
#' # ======================
#' # Run with FIML (default) uses all information
#' umxGxE_window(selDVs = selDVs, moderator = moderator, mzData = mzData, dzData = dzData);
#' 
#' # Run creating weighted covariance matrices (excludes missing data)
#' umxGxE_window(selDVs = selDVs, moderator = moderator, mzData = mzData, dzData = dzData, weightCov = TRUE); 
#' 
#' # Run and plot for specified windows (in this case just 1927)
#' umxGxE_window(selDVs = selDVs, moderator = moderator, mzData = mzData, dzData = dzData, specifiedTargets = 1927, plotWindow = T)
#' 
#' @family umx twin modeling
#' @references - Hildebrandt, A., Wilhelm, O, & Robitzsch, A. (2009)
#' Complementary and competing factor analytic approaches for the investigation 
#' of measurement invariance. \emph{Review of Psychology}, \bold{16}, 87--107. 
#' 
#' Briley, D, Bates, T.C., Harden, K Tucker-Drob, E. (2015)
#' Of mice and men: Local SEM in gene environment analysis. \emph{Behavior Genetics}.

umxGxE_window <- function(selDVs = NULL, moderator = NULL, mzData = mzData, dzData = dzData, weightCov = F, specifiedTargets = NULL, width = 1, plotWindow = F, return = c("estimates","last_model")) {
	# TODO want to allow missing moderator?
	# Check moderator is set and exists in mzData and dzData
	if(is.null(moderator)){
		stop("Moderator must be set to the name of the moderator column, e.g, moderator = \"birth_year\"")
	}
	# Check DVs exists in mzData and dzData (and nothing else apart from the moderator)
	umx_check_names(c(selDVs, moderator), data = mzData, die = T, no_others = T)
	umx_check_names(c(selDVs, moderator), data = dzData, die = T, no_others = T)

	# Add a zygosity column (that way we know what it's called)
	mzData$ZYG = "MZ"; 
	dzData$ZYG = "DZ"
	# If using cov.wt, remove missings
	if(weightCov){
		dz.complete = complete.cases(dzData)
		if(sum(dz.complete) != nrow(dzData)){
			message("removed ", nrow(dzData) - sum(dz.complete), " cases from DZ data due to missingness. To use incomplete data, set weightCov = FALSE")
			dzData = dzData[dz.complete, ]
		}
		mz.complete = complete.cases(mzData)
		if(sum(mz.complete) != nrow(mzData)){
			message("removed ", nrow(mzData) - sum(mz.complete), " cases from MZ data due to missingness. To use incomplete data, set weightCov = FALSE")
			mzData = mzData[mz.complete, ]
		}
	}
	allData = rbind(mzData, dzData)
	# Create range of moderator values to iterate over (using the incoming moderator variable name)
	modVar  = allData[, moderator]
	if(any(is.na(modVar))){		
		stop("Moderator \"", moderator, "\" contains ", length(modVar[is.na(modVar)]), "NAs. This is not currently supported.\n",
			"NA found on rows", paste(which(is.na(modVar)), collapse = ", "), " of the combined data."
		)
	}

	if(!is.null(specifiedTargets)){
		if(specifiedTargets < min(modVar)) {
			stop("specifiedTarget is below the range in moderator. min(modVar) was ", min(modVar))
		} else if(specifiedTargets > max(modVar)){
			stop("specifiedTarget is above the range in moderator. max(modVar) was ", max(modVar))
		} else {
			targetLevels = specifiedTargets			
		}
	} else {
		# by default, run across each integer value of the moderator
		targetLevels = seq(min(modVar), max(modVar))
	}

	numPairs     = nrow(allData)
	moderatorSD  = sd(modVar, na.rm = T)
	bw           = 2 * numPairs^(-.2) * moderatorSD *  width # -.2 == -1/5 

	ACE = c("A","C","E")
	tmp = rep(NA, length(targetLevels))
	out = data.frame(modLevel = targetLevels, Astd = tmp, Cstd = tmp, Estd = tmp, A = tmp, C = tmp, E = tmp)
	n   = 1
	for (i in targetLevels) {
		# i = targetLevels[1]
		message("mod = ", i)
		zx = (modVar - i)/bw
		k = (1 / (2 * pi)^.5) * exp((-(zx)^2) / 2)
		# ======================================================
		# = Insert the weights into the dataframes as "weight" =
		# ======================================================
		allData$weight = k/.399 
		mzData = subset(allData, ZYG == "MZ", c(selDVs, "weight"))
		dzData = subset(allData, ZYG == "DZ", c(selDVs, "weight"))
		if(weightCov){
			mz.wt = cov.wt(mzData[, selDVs], mzData$weight)
			dz.wt = cov.wt(dzData[, selDVs], dzData$weight)
			m1 = umxACE(selDVs = selDVs, dzData = dz.wt$cov, mzData = mz.wt$cov, numObsDZ = dz.wt$n.obs, numObsMZ = mz.wt$n.obs)
		} else {
			m1 = umxACE(selDVs = selDVs, dzData = dzData, mzData = mzData, weightVar = "weight")
		}
		m1  = mxRun(m1); 
		if(plotWindow){
			plot(allData[,moderator], allData$weight) # normal-curve yumminess
			umxSummaryACE(m1)
		}
		out[n, ] = mxEval(c(i, top.a_std[1,1], top.c_std[1,1],top.e_std[1,1], top.a[1,1], top.c[1,1], top.e[1,1]), m1)
		n = n + 1
	}
	# squaring paths to produce variances
	out[,ACE] <- out[,ACE]^2
	# plotting variance components
	with(out,{
		plot(A ~ modLevel, main = paste0(selDVs[1], " variance"), ylab = "Variance", xlab=moderator, las = 1, bty = 'l', type = 'l', col = 'red', ylim = c(0, 1), data = out)
		lines(modLevel, C, col = 'green')
		lines(modLevel, E, col = 'blue')
		legend('topright', fill = c('red', 'green', 'blue'), legend = ACE, bty = 'n', cex = .8)

		plot(Astd ~ modLevel, main = paste0(selDVs[1], "std variance"), ylab = "Std Variance", xlab=moderator, las = 1, bty = 'l', type = 'l', col = 'red', ylim = c(0, 1), data = out)
		lines(modLevel, Cstd, col = 'green')
		lines(modLevel, Estd, col = 'blue')
		legend('topright', fill = c('red', 'green', 'blue'), legend = ACE, bty = 'n', cex = .8)
	})
	if(return == "last_model"){
		invisible(m1)
	} else if(return == "estimates") {
		invisible(out)
	}else{
		warning("You specified a return type that is invalid. Valid options are last_model and estimates. You requested:", return)
	}
}

#' umxACE
#'
#' Make a 2-group ACE model
#'
#' @param name The name of the model (defaults to"ACE")
#' @param selDVs The variables to include from the data
#' @param dzData The DZ dataframe
#' @param mzData The MZ dataframe
#' @param numObsDZ = Number of DZ twins: Set this if you input covariance data
#' @param numObsMZ = Number of MZ twins: Set this if you input covariance data
#' @param weightVar = If provided, a vector objective used to weight the data. (default = NULL) 
#' @return - \code{\link{mxModel}}
#' @export
#' @family umx twin modeling
#' @references - \url{http://github.com/tbates/umx}
#' @examples
#' require(OpenMx)
#' require(umx)
#' data(twinData)
#' twinData$ZYG = factor(twinData$zyg, levels = 1:5, labels = c("MZFF", "MZMM", "DZFF", "DZMM", "DZOS"))
#' selDVs = c("bmi1","bmi2")
#' mzData <- as.matrix(subset(twinData, ZYG == "MZFF", selDVs))
#' dzData <- as.matrix(subset(twinData, ZYG == "DZFF", selDVs))
#' m1 = umxACE(selDVs = selDVs, dzData = dzData, mzData = mzData)
#' m1 = umxRun(m1)
#' umxSummaryACE(m1)
umxACE <- function(name = "ACE", selDVs, dzData, mzData, numObsDZ = NULL, numObsMZ = NULL, weightVar = NULL) {
	nSib = 2
	nVar = length(selDVs)/nSib; # number of dependent variables ** per INDIVIDUAL ( so times-2 for a family)**
	equateMeans = T
	dzAr = .5
	dzCr = 1
	addStd = T
	boundDiag = NULL
	dataType = umx_is_cov(dzData)
	bVector = FALSE	
	if(dataType == "raw") {
		if(!all(is.null(c(numObsMZ, numObsDZ)))){
			stop("You should not be setting numObs with ", dataType, " data…")
		}
		if(!is.null(weightVar)){
			# weight variable provided: check it exists in each frame
			if(!umx_check_names(weightVar, data = mzData, die=F) | !umx_check_names(weightVar, data = dzData, die=F)){
				stop("The weight variable must be included in the mzData and dzData",
					 " frames passed into umxACE when \"weightVar\" is specified",
					 "\n mzData contained:", paste(names(mzData), collapse = ", "),
					 "\n and dzData contain:", paste(names(dzData), collapse = ", "),
					 "\nbut I was looking for ", weightVar, " as the moderator name"
				)
			}
			mzWeightMatrix = mxMatrix(name = "mzWeightMatrix", type = "Full", nrow = nrow(mzData), ncol = 1, free = F, values = mzData[, weightVar])
			dzWeightMatrix = mxMatrix(name = "dzWeightMatrix", type = "Full", nrow = nrow(dzData), ncol = 1, free = F, values = dzData[, weightVar])
			mzData = mzData[, selDVs]
			dzData = dzData[, selDVs]
			bVector = TRUE
		} else {
			# no weights
		}
		# Add means matrix to top
		obsMZmeans = colMeans(mzData[, selDVs], na.rm = T);
		top = mxModel(name = "top", 
			# means (not yet equated across twins)
			umxLabel(mxMatrix(name = "expMean", "Full" , nrow = 1, ncol = (nVar * 2), free = T, values = obsMZmeans, dimnames = list("means", selDVs)) )
		)
		modelMZ = mxModel(name = "MZ", 
			# TODO swap these back for 2.0...
			mxFIMLObjective("top.mzCov", "top.expMean", vector = bVector),
			# mxExpectationNormal("top.mzCov", "top.expMean"), mxFitFunctionFIML(vector = bVector),
			mxData(mzData, type = "raw")
		)
		modelDZ = mxModel(name = "DZ", 
			# TODO swap these back for 2.0...
			mxFIMLObjective("top.dzCov", "top.expMean", vector = bVector),
			# mxExpectationNormal("top.dzCov", "top.expMean"), mxFitFunctionFIML(vector = bVector),
			mxData(dzData, type = "raw")
		)
	} else if(dataType %in% c("cov", "cor")){
		if(!is.null(weightVar)){
			stop("You can't set weightVar when you give cov data - use cov.wt to create weighted cov matrices or pass in raw data")
		}
		if( is.null(numObsMZ)){ stop(paste0("You must set numObsMZ with ", dataType, " data"))}
		if( is.null(numObsDZ)){ stop(paste0("You must set numObsDZ with ", dataType, " data"))}
		het_mz = umx_reorder(mzData, selDVs)		
		het_dz = umx_reorder(dzData, selDVs)

		top = mxModel(name = "top")

		modelMZ = mxModel(name = "MZ", 
			mxMLObjective("top.mzCov"),
			# TODO swap these back for 2.0...
			# mxExpectationNormal("top.mzCov"), mxFitFunctionML(),			
			mxData(mzData, type = "cov", numObs = numObsMZ)
		)
		modelDZ = mxModel(name = "DZ", 
			mxMLObjective("top.dzCov"),
			# TODO swap these back for 2.0...
			# mxExpectationNormal("top.dzCov"), mxFitFunctionML(),
			mxData(dzData, type = "cov", numObs = numObsDZ)
		)
		message("treating data as ", dataType)
	} else {
		stop("Datatype \"", dataType, "\" not understood")
	}
	# Finish building top
	top = mxModel(top,
		# "top" defines the algebra of the twin model, which MZ and DZ slave off of
		# NB: top already has the means model added if raw  - see above
		umxLabel(mxMatrix(name = "a", type = "Lower", nrow = nVar, ncol = nVar, free = T, values = .6, byrow = T), jiggle = 0.05),  # Additive genetic path 
		umxLabel(mxMatrix(name = "c", type = "Lower", nrow = nVar, ncol = nVar, free = T, values = .3, byrow = T), jiggle = 0.05),  # Common environmental path 
		umxLabel(mxMatrix(name = "e", type = "Lower", nrow = nVar, ncol = nVar, free = T, values = .6, byrow = T), jiggle = 0.05),  # Unique environmental path
		mxMatrix(name = "dzAr", type = "Full", 1, 1, free = F, values = dzAr),
		mxMatrix(name = "dzCr", type = "Full", 1, 1, free = F, values = dzCr),
		# Multiply by each path coefficient by its inverse to get variance component
		# Quadratic multiplication to add common_loadings
		mxAlgebra(a %*% t(a), name = "A"), # additive genetic variance
		mxAlgebra(c %*% t(c), name = "C"), # common environmental variance
		mxAlgebra(e %*% t(e), name = "E"), # unique environmental variance
		mxAlgebra(A+C+E     , name = "ACE"),
		mxAlgebra(A+C       , name = "AC" ),
		mxAlgebra( (dzAr %x% A) + (dzCr %x% C),name = "hAC"),
		mxAlgebra(rbind (cbind(ACE, AC), 
		                 cbind(AC , ACE)), dimnames = list(selDVs, selDVs), name = "mzCov"),
		mxAlgebra(rbind (cbind(ACE, hAC),
		                 cbind(hAC, ACE)), dimnames = list(selDVs, selDVs), name = "dzCov")
	)

	if(!bVector){
		model = mxModel(name, modelMZ, modelDZ, top,
			mxAlgebra(MZ.objective + DZ.objective, name = "twin"),
			# TODO fix this for OpenMx 2
			mxAlgebraObjective("twin")
			# mxFitFunctionAlgebra("twin")
		)
	} else {
		# bVector is TRUE!
		# To weight objective functions in OpenMx, you specify a container model that applies the weights
		# m1 is the model with no weights, but with "vector = TRUE" option added to the FIML objective.
		# This option makes FIML return individual likelihoods for each row of the data (rather than a single -2LL value for the model)
		# You then optimize weighted versions of these likelihoods
		# by building additional models containing weight data and an algebra that multiplies the likelihoods from the first model by the weight vector
		model = mxModel(name, modelMZ, modelDZ, top,
			mxModel("MZw", mzWeightMatrix,
				mxAlgebra(-2 * sum(mzWeightMatrix * log(MZ.objective) ), name = "mzWeightedCov"),
				# TODO fix this for OpenMx 2
				mxAlgebraObjective("mzWeightedCov")
				# mxFitFunctionAlgebra("mzWeightedCov")
			),
			mxModel("DZw", dzWeightMatrix,
				mxAlgebra(-2 * sum(dzWeightMatrix * log(DZ.objective) ), name = "dzWeightedCov"),
				# TODO fix this for OpenMx 2
				mxAlgebraObjective("dzWeightedCov")
			    # mxFitFunctionAlgebra("dzWeightedCov")
			),
			mxAlgebra(MZw.objective + DZw.objective, name = "twin"),
			# TODO fix this for OpenMx 2
			mxAlgebraObjective("twin")
		    # mxFitFunctionAlgebra("twin")
		)
	}

	diag(model@submodels$top@matrices$a@lbound) = 0
	diag(model@submodels$top@matrices$c@lbound) = 0
	diag(model@submodels$top@matrices$e@lbound) = 0
	newTop = mxModel(model@submodels$top, 
		mxMatrix("Iden", nVar, nVar, name = "I"), # nVar Identity matrix
		mxAlgebra(A+C+E, name = "Vtot"),          # Total variance
		mxAlgebra(solve(sqrt(I * Vtot)), name = "SD"), # Total variance
		mxAlgebra(SD %*% a, name = "a_std"), # standardized a
		mxAlgebra(SD %*% c, name = "c_std"), # standardized c
		mxAlgebra(SD %*% e, name = "e_std")  # standardized e
	)
	model  = mxModel(model, newTop, mxCI(c('top.a_std','top.c_std','top.e_std')))
	# Equate means for twin1 and twin 2 by matching labels in the first and second halves of the means labels matrix
	if(dataType == "raw"){
		model = omxSetParameters(model,
			labels  = paste0("expMean_r1c", (nVar+1):(nVar*2)), # c("expMean14", "expMean15", "expMean16"),
			newlabels = paste0("expMean_r1c", 1:nVar)           # c("expMean11", "expMean12", "expMean13")
		)
	}
	# Just trundle through and make sure values with the same label have the same start value... means for instance.
	model = omxAssignFirstParameters(model)
	return(model)
}
# ========================================
# = Model building and modifying helpers =
# ========================================

#' umxValues
#'
#' umxValues will set start values for the free parameters in RAM and Matrix \code{\link{mxModel}}s, or even mxMatrices.
#' It will try and be smart in guessing these from the values in your data, and the model type.
#' If you give it a numeric input, it will use obj as the mean, return a list of length n, with sd = sd
#'
#' @param obj The RAM or matrix \code{\link{mxModel}}, or \code{\link{mxMatrix}} that you want to set start values for.
#' @param sd Optional Standard Deviation for start values
#' @param n  Optional Mean for start values
#' @param onlyTouchZeros Don't start things that appear to have already been started (useful for speeding \code{\link{umxReRun}})
#' @return - \code{\link{mxModel}} with updated start values
#' @export
#' @seealso - Core functions:
#' @family umx core functions
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = F, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' mxEval(S, m1) # default variances are 0
#' m1 = umxValues(m1)
#' mxEval(S, m1) # plausible variances
#' umxValues(14, sd = 1, n = 10) # return vector of length 10, with mean 14 and sd 1
#' # # TODO get this working
#' # umx_print(mxEval(S,m1), 2, zero.print= ".") # plausible variances

umxValues <- function(obj = NA, sd = NA, n = 1, onlyTouchZeros = FALSE) {
	if(is.numeric(obj) ) {
		# use obj as the mean, return a list of length n, with sd = sd
		return(xmuStart_value_list(x = obj, sd = sd, n = n))
	} else {
		if (!umx_is_RAM(obj) ) {
			stop("'obj' must be a RAM model (or a simple number)")
		}
		# This is a RAM Model: Set sane starting values
		# Means at manifest means
		# S at variance on diag, quite a bit less than cov off diag
		# TODO: Start values in the A matrix...
		# TODO: Start latent means?...		
		# TODO: Handle sub models...
		if (length(obj@submodels) > 0) {
			stop("Cannot yet handle submodels")
		}
		if (is.null(obj@data)) {
			stop("'model' does not contain any data")
		}
		if(!is.null(obj@matrices$Thresholds)){
			message("this is a threshold RAM model... I'm not sure how to handle setting values in these yet")
			return(obj)
		}
		theData   = obj@data@observed
		manifests = obj@manifestVars
		latents   = obj@latentVars
		nVar      = length(manifests)

		if(length(latents) > 0){
			lats  =  (nVar+1):(nVar + length(latents))
			# The diagonal is variances
			if(onlyTouchZeros) {
				freePaths = (obj@matrices$S@free[lats, lats] == TRUE) & obj@matrices$S@values[lats, lats] == 0
			} else {
				freePaths = (obj@matrices$S@free[lats, lats] == TRUE)			
			}
			obj@matrices$S@values[lats, lats][freePaths] = 1
			offDiag = !diag(length(latents))
			newOffDiags = obj@matrices$S@values[lats, lats][offDiag & freePaths]/3
			obj@matrices$S@values[lats, lats][offDiag & freePaths] = newOffDiags			
		}

		# =============
		# = Set means =
		# =============
		if(obj@data@type == "raw"){
			# = Set the means =
			if(is.null(obj@matrices$M)){
				msg("You are using raw data, but have not yet added paths for the means\n")
				stop("You do this with mxPath(from = 'one', to = 'var')")
			} else {			
				dataMeans = colMeans(theData[,manifests], na.rm = T)
				freeManifestMeans = (obj@matrices$M@free[1, manifests] == T)
				obj@matrices$M@values[1, manifests][freeManifestMeans] = dataMeans[freeManifestMeans]
				covData = cov(theData, use = "pairwise.complete.obs")
			}
		} else {
			covData = theData
		}
		dataVariances = diag(covData)
		# ======================================================
		# = Fill the symmetrical matrix with good start values =
		# ======================================================
		# The diagonal is variances
		if(onlyTouchZeros) {
			freePaths = (obj@matrices$S@free[1:nVar, 1:nVar] == TRUE) & obj@matrices$S@values[1:nVar, 1:nVar] == 0
		} else {
			freePaths = (obj@matrices$S@free[1:nVar, 1:nVar] == TRUE)			
		}
		obj@matrices$S@values[1:nVar, 1:nVar][freePaths] = (covData[freePaths]/2)
		offDiag = !diag(nVar)
		newOffDiags = obj@matrices$S@values[1:nVar, 1:nVar][offDiag & freePaths]/3
		obj@matrices$S@values[1:nVar, 1:nVar][offDiag & freePaths] = newOffDiags

		# ==========================================
		# = Put modest starts into the asymmetrics =
		# ==========================================
		Arows = nrow(obj@matrices$A@free)
		Acols = ncol(obj@matrices$A@free)
		if(onlyTouchZeros) {
			freePaths = (obj@matrices$A@free[1:Arows, 1:Acols] == TRUE) & obj@matrices$A@values[1:Arows, 1:Acols] == 0
		} else {
			freePaths = (obj@matrices$A@free[1:Arows, 1:Acols] == TRUE)			
		}
		obj@matrices$A@values[1:Arows, 1:Acols][freePaths] = .9
		return(obj)
	}
}

#' umxLabel
#'
#' umxLabel adds labels to things, be it an: \code{\link{mxModel}} (RAM or matrix based), an \code{\link{mxPath}}, or an \code{\link{mxMatrix}}
#' This is a core function in umx: Adding labels to paths opens the door to \code{\link{umxEquate}}, as well as \code{\link{omxSetParameters}}
#'
#' @param obj An \code{\link{mxModel}} (RAM or matrix based), \code{\link{mxPath}}, or \code{\link{mxMatrix}}
#' @param suffix String to append to each label (might be used to distinguish, say male and female submodels in a model)
#' @param baseName String to prepend to labels. Defaults to NA ("")
#' @param setfree Whether to label only the free paths (defaults to FALSE)
#' @param drop The value to fix "drop" paths to (defaults to 0)
#' @param jiggle How much to jiggle values in a matrix or list of path values
#' @param labelFixedCells = TRUE
#' @param boundDiag Whether to bound the diagonal of a matrix
#' @param verbose How much feedback to give the user (default = FALSE)
#' @param overRideExisting = FALSE
#' 
#' @return - \code{\link{mxModel}}
#' @export
#' @family umx core functions
#' @seealso - \code{\link{umxGetParameters}}, \code{\link{umxRun}}, \code{\link{umxValues}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @export
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = F, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' umxGetParameters(m1) # Only "matrix address" labels: "One Factor.S[2,2]" "One Factor.S[3,3]"
#' m1 = umxLabel(m1)
#' umxGetParameters(m1, free = TRUE) # Infomative labels: "G_to_x1", "x4_with_x4", etc.
#' # Labeling a matrix
#' a = umxLabel(mxMatrix("Full", 3,3, values = 1:9, name = "a"))
#' a$labels
umxLabel <- function(obj, suffix = "", baseName = NA, setfree = F, drop = 0, labelFixedCells = T, jiggle = NA, boundDiag = NA, verbose = F, overRideExisting = F) {	
	# TODO change to umxSetLabels?
	if (is(obj, "MxMatrix") ) { 
		# Label an mxMatrix
		xmuLabel_Matrix(mx_matrix = obj, baseName = baseName, setfree = setfree, drop = drop, jiggle = jiggle, boundDiag = boundDiag, suffix = suffix, verbose = verbose, labelFixedCells = labelFixedCells)
	} else if (umx_is_RAM(obj)) { 
		# Label a RAM model
		if(verbose){message("RAM")}
		return(xmuLabel_RAM_Model(model = obj, suffix = suffix, labelFixedCells = labelFixedCells, overRideExisting = overRideExisting, verbose = verbose))
	} else if (umx_is_MxModel(obj) ) {
		# Label a non-RAM matrix lamodel
		return(xmuLabel_MATRIX_Model(model = obj, suffix = suffix, verbose= verbose))
	} else {
		stop("I can only label OpenMx models and mxMatrix types. You gave me a ", typeof(obj))
	}
}

# =================================
# = Run Helpers =
# =================================

#' umxRun
#'
#' umxRun is a version of \code{\link{mxRun}} which can run also set start values, labels, and run multiple times
#' It can also calculate the saturated and independence likelihoods necessary for most fit indices.
#'
#' @param model The \code{\link{mxModel}} you wish to run.
#' @param n The maximum number of times you want to run the model trying to get a code green run (defaults to 1)
#' @param calc_SE Whether to calculate standard errors (not used when n = 1)
#' for the summary (they are not very accurate, so if you use \code{\link{mxCI}} or \code{\link{umxCI}}, you can turn this off)
#' @param calc_sat Whether to calculate the saturated and independence models (for raw \code{\link{mxData}} \code{\link{mxModel}}s) (defaults to TRUE - why would you want anything else?)
#' @param setValues Whether to set the starting values of free parameters (defaults to F)
#' @param setLabels Whether to set the labels (defaults to F)
#' @param intervals Whether to run mxCI confindence intervals (defaults to F)
#' @param comparison Whether to run umxCompare() after umxRun
#' @param setStarts Deprecated way to setValues
#' @return - \code{\link{mxModel}}
#' @family umx core functions
#' @seealso - \code{\link{mxRun}}, \code{\link{umxLabel}}, \code{\link{umxValues}}, \code{\link{umxReRun}}, \code{\link{confint}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @export
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = F, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1) # just run: will create saturated model if needed
#' m1 = umxRun(m1, setValues = T, setLabels = T) # set start values and label all parameters
#' umxSummary(m1, show = "std")
#' m1 = mxModel(m1, mxCI("G_to_x1")) # add one CI
#' m1 = mxRun(m1, intervals = TRUE)
#' residuals(m1, run = TRUE) # get CIs on all free parameters
#' confint(m1, run = TRUE) # get CIs on all free parameters
#' m1 = umxRun(m1, n = 10) # re-run up to 10 times if not green on first run
umxRun <- function(model, n = 1, calc_SE = TRUE, calc_sat = TRUE, setValues = FALSE, setLabels = FALSE, intervals = FALSE, comparison = NULL, setStarts = NULL){
	# TODO: return change in -2LL for models being re-run
	# TODO: stash saturated model for re-use
	# TODO: Optimise for speed
	if(!is.null(setStarts)){
		message("change setStarts to setValues (more consistent)")
		setValues = setStarts
	}
	if(setLabels){
		model = umxLabel(model)
	}
	if(setValues){
		model = umxValues(model)
	}
	if(n == 1){
		model = mxRun(model, intervals = intervals);
	} else {
		model = mxOption(model, "Calculate Hessian", "No")
		model = mxOption(model, "Standard Errors", "No")
		# make an initial run
		model = mxRun(model);
		n = (n - 1); tries = 0
		# carry on if we failed
		while(model@output$status[[1]] == 6 && n > 2 ) {
			print(paste("Run", tries+1, "status Red(6): Trying hard...", n, "more times."))
			model <- mxRun(model)
			n <- (n - 1)
			tries = (tries + 1)
		}
		if(tries == 0){ 
			# print("Ran fine first time!")	
		}
		# get the SEs for summary (if requested)
		if(calc_SE){
			# print("Calculating Hessian & SEs")
			model = mxOption(model, "Calculate Hessian", "Yes")
			model = mxOption(model, "Standard Errors", "Yes")
		}
		if(calc_SE | intervals){
			model = mxRun(model, intervals = intervals)
		}
	}
	if( umx_is_RAM(model)){
		if(model@data@type == "raw"){
			# If we have a RAM model with raw data, compute the satuated and indpendence models
			# TODO: Update to omxSaturated() and omxIndependenceModel()
			message("computing saturated and independence models so you have access to absoute fit indices for this raw-data model")
			model_sat = umxSaturated(model, evaluate = T, verbose = F)
			model@output$IndependenceLikelihood = as.numeric(-2 * logLik(model_sat$Ind))
			model@output$SaturatedLikelihood    = as.numeric(-2 * logLik(model_sat$Sat))
		}
	}
	if(!is.null(comparison)){ 
		umxCompare(comparison, model) 
	}
	return(model)
}

#' umxReRun
#' 
#' umxReRun Is a convenience function to re-run an \code{\link{mxModel}}, optionally adding, setting, or dropping parameters.
#' The main value for umxReRun is compactness. So this one-liner drops a path labelled "Cs", and returns the updated model:
#' fit2 = umxReRun(fit1, update = "Cs", name = "newModelName", comparison = T)
#' A powerful feature is regular expression. These let you drop collections of paths by matching patterns
#' fit2 = umxReRun(fit1, update = "C[sr]", regex = TRUE, name = "drop_Cs_andCr", comparison = T)
#' 
#' If you're just starting out, you might find it easier to be more explicit. Like this: 
#' 
#' fit2 = omxSetParameters(fit1, labels = "Cs", values = 0, free = F, name = "newModelName")
#' 
#' fit2 = mxRun(fit2)
#' 
#' @param lastFit  The \code{\link{mxModel}} you wish to update and run.
#' @param update What to update before re-running. Can be a list of labels, a regular expression (set regex = T) or an object such as mxCI etc.
#' @param regex    Whether or not update is a regular expression (defaults to FALSE)
#' @param free     The state to set "free" to for the parameters whose labels you specify (defaults to free = FALSE, i.e., fixed)
#' @param value    The value to set the parameters whose labels you specify too (defaults to 0)
#' @param freeToStart Whether to update parameters based on their current free-state. free = c(TRUE, FALSE, NA), (defaults to NA - i.e, not checked)
#' @param name      The name for the new model
#' @param verbose   How much feedback to give
#' @param intervals Whether to run confidence intervals (see \code{\link{mxRun}})
#' @param comparison Whether to run umxCompare() after umxRun
#' @param dropList A list of strings. If not NA, then the labels listed here will be dropped (or set to the value and free state you specify)
#' @return - \code{\link{mxModel}}
#' @family umx core functions
#' @seealso - \code{\link{mxRun}}, \code{\link{umxRun}}, \code{\link{omxGetParameters}}
#' @references - \url{http://github.com/tbates/umx}
#' @export
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = F, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = T, setValues = T)
#' m2 = umxReRun(m1, update = "G_to_x1", name = "drop_X1")
#' umxSummary(m2); umxCompare(m1, m2)
#' # 1-line version including comparison
#' m2 = umxReRun(m1, update = "G_to_x1", name = "drop_X1", comparison = T)
#' m2 = umxReRun(m1, update = "^G_to_x[3-5]", regex = T, name = "drop_G_to_x3_x4_and_x5", comparison = T)
#' m2 = umxReRun(m1, update = "G_to_x1", value = .2, name = "fix_G_x1_at_point2", comparison = T)
#' m3 = umxReRun(m2, update = "G_to_x1", free=TRUE, name = "free_G_x1_again")
#' umxCompare(m3, m2)

umxReRun <- function(lastFit, update = NA, regex = FALSE, free = FALSE, value = 0, freeToStart = NA, name = NULL, verbose = FALSE, intervals = FALSE, comparison = FALSE, dropList = "deprecated") {
	if (dropList != "deprecated" | typeof(regex) != "logical"){
		if(dropList != "deprecated"){
			stop("hi. Sorry for the change, but please replace ", omxQuotes("dropList"), " with ", omxQuotes("update"),". e.g.:\n",
				"umxReRun(m1, dropList = ", omxQuotes("E_to_heartRate"), ")\n",
				"becomes\n",
				"umxReRun(m1, update = ", omxQuotes("E_to_heartRate"), ")\n"
			)
		} else {
			stop("hi. Sorry for the change, but to use regex replace ", omxQuotes("regex"), " with ", omxQuotes("update"),
			 "AND regex =", omxQuotes(F), "e.g.:\n",
			 "umxReRun(m1, regex = ", omxQuotes("^E_.*"), ")\n",
			 "becomes\n",
			 "umxReRun(m1, update = ", omxQuotes("^E_.*"), ", regex = TRUE)\n"
			 )
		}
	}

	if(is.null(name)){ name = lastFit@name }

	if(regex | typeof(update) == "character") {
		if (regex) {
			theLabels = umxGetParameters(lastFit, regex = update, free = freeToStart, verbose = verbose)
		}else {
			theLabels = update
		}
		x = omxSetParameters(lastFit, labels = theLabels, free = free, value = value, name = name)		
	} else {
		# TODO Label and start new object new object
		x = mxModel(lastFit, update, name = name)
	}
	x = mxRun(x, intervals = intervals)
	if(comparison){
		print(umxCompare(lastFit, x))
	}
	return(x)
}

# =====================================
# = Parallel helpers to be added here =
# =====================================

#' umxStandardizeModel
#'
#' umxStandardizeModel takes a RAM-style model, and returns standardized version.
#'
#' @param model The \code{\link{mxModel}} you wish to standardise
#' @param return What to return. Valid options: "parameters", "matrices", or "model"
#' @param Amatrix Optionally tell the function what the name of the asymmetric matrix is (defaults to RAM standard A)
#' @param Smatrix Optionally tell the function what the name of the symmetric matrix is (defaults to RAM standard S)
#' @param Mmatrix Optionally tell the function what the name of the means matrix is (defaults to RAM standard M)
#' @return - a \code{\link{mxModel}} or else parameters or matrices if you request those
#' @family umx reporting functions
#' @references - \url{http://github.com/tbates/umx}
#' @export
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = F, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = T, setValues = T)
#' m1 = umxStandardizeModel(m1, return = "model")
#' summary(m1)
umxStandardizeModel <- function(model, return = "parameters", Amatrix = NA, Smatrix = NA, Mmatrix = NA) {
	if (!(return == "parameters"|return == "matrices"|return == "model")) stop("Invalid 'return' parameter. Do you want do get back parameters, matrices or model?")
	suppliedNames = all(!is.na(c(Amatrix,Smatrix)))
	# if the objective function isn't RAMObjective, you need to supply Amatrix and Smatrix

	if (!umx_is_RAM(model) & !suppliedNames ){
		stop("I need either type = RAM model or the names of the equivalent of the A and S matrices.")
	}
	output <- model@output
	# Stop if there is no objective function
	if (is.null(output))stop("Provided model has no objective function, and thus no output. I can only standardize models that have been run!")
	# Stop if there is no output
	if (length(output) < 1){
		message("Model has not been run yet")
		return(model)
	}
	# Get the names of the A, S and M matrices 
	if("expectation" %in% slotNames(model)){
		# openMx 2
		if (is.character(Amatrix)){nameA <- Amatrix} else {nameA <- model@expectation@A}
		if (is.character(Smatrix)){nameS <- Smatrix} else {nameS <- model@expectation@S}
		if (is.character(Mmatrix)){nameM <- Mmatrix} else {nameM <- model@expectation@M}
	} else {
		if (is.character(Amatrix)){nameA <- Amatrix} else {nameA <- model@objective@A}
		if (is.character(Smatrix)){nameS <- Smatrix} else {nameS <- model@objective@S}
		if (is.character(Mmatrix)){nameM <- Mmatrix} else {nameM <- model@objective@M}
	}
	# Get the A and S matrices, and make an identity matrix
	A <- model[[nameA]]
	S <- model[[nameS]]
	I <- diag(nrow(S@values))
	
	# this can fail (non-invertable etc. so we wrap it in try-catch)
	tryCatch({	
		# Calculate the expected covariance matrix
		IA <- solve(I - A@values)
		expCov <- IA %*% S@values %*% t(IA)
		# Return 1/SD to a diagonal matrix
		invSDs <- 1/sqrt(diag(expCov))
		# Give the inverse SDs names, because mxSummary treats column names as characters
		names(invSDs) <- as.character(1:length(invSDs))
		if (!is.null(dimnames(A@values))){names(invSDs) <- as.vector(dimnames(S@values)[[2]])}
		# Put the inverse SDs into a diagonal matrix (might as well recycle my I matrix from above)
		diag(I) <- invSDs
		# Standardize the A, S and M matrices
		#  A paths are value*sd(from)/sd(to) = I %*% A %*% solve(I)
		#  S paths are value/(sd(from*sd(to))) = I %*% S %*% I
		stdA <- I %*% A@values %*% solve(I)
		stdS <- I %*% S@values %*% I
		# Populate the model
		model[[nameA]]@values[,] <- stdA
		model[[nameS]]@values[,] <- stdS
		if (!is.na(nameM)){model[[nameM]]@values[,] <- rep(0, length(invSDs))}
	}, warning = function(cond) {
	    # warning-handler-code
        message(cond)
	}, error = function(cond) {
	    cat("The model could not be standardized")
        message(cond)
	}, finally = {
	    # cleanup-code
	})

	# Return the model, if asked
	if(return=="model"){
		return(model)
	}else if(return=="matrices"){
		# return the matrices, if asked
		matrices <- list(model[[nameA]], model[[nameS]])
		names(matrices) <- c("A", "S")
		return(matrices)
	}else if(return == "parameters"){
		# return the parameters
		#recalculate summary based on standardised matrices
		p <- summary(model)$parameters
		p <- p[(p[,2] == nameA)|(p[,2] == nameS),]
		## get the rescaling factor
		# this is for the A matrix
		rescale <- invSDs[p$row] * 1/invSDs[p$col]
		# this is for the S matrix
		rescaleS <- invSDs[p$row] * invSDs[p$col]
		# put the A and the S together
		rescale[p$matrix == "S"] <- rescaleS[p$matrix == "S"]
		# rescale
		p[,5] <- p[,5] * rescale
		p[,6] <- p[,6] * rescale
		# rename the columns
		# names(p)[5:6] <- c("Std. Estimate", "Std.Std.Error")
		return(p)		
	}
}

# ==============================
# = Label and equate functions =
# ==============================

#' umxGetParameters
#'
#' Get the parameter labels from a model. Like \code{\link{omxGetParameters}},
#' but supercharged with regular expressions for more power and ease!
#'
#' @param inputTarget An object to get parameters from: could be a RAM \code{\link{mxModel}}
#' @param regex A regular expression to filter the labels defaults to NA - just returns all labels)
#' @param free  A Boolean determining whether to return only free parameters.
#' @param verbose How much feedback to give
#' @export
#' @family umx model updating and comparison
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = F, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1)
#' umxGetParameters(m1)
#' m1 = umxRun(m1, setLabels = T)
#' umxGetParameters(m1)
#' umxGetParameters(m1, free=T) # only the free parameter
#' umxGetParameters(m1, free=F) # only parameters which are fixed
#' \dontrun{
#' # Complex regex patterns
#' umxGetParameters(m2, regex = "S_r_[0-9]c_6", free = T) # Column 6 of matrix "as"
#' }
umxGetParameters <- function(inputTarget, regex = NA, free = NA, verbose = F) {
	# TODO Be nice to offer a method to handle submodels
	# model@submodels$aSubmodel@matrices$aMatrix@labels
	# model@submodels$MZ@matrices
	if(umx_is_MxModel(inputTarget)) {
		topLabels = names(omxGetParameters(inputTarget, indep = FALSE, free = free))
	} else if(is(inputTarget, "MxMatrix")) {
		if(is.na(free)) {
			topLabels = inputTarget@labels
		} else {
			topLabels = inputTarget@labels[inputTarget@free==free]
		}
	}else{
		stop("I am sorry Dave, umxGetParameters needs either a model or an mxMatrix: you offered a ", class(inputTarget)[1])
	}
	theLabels = topLabels[which(!is.na(topLabels))] # exclude NAs
	if( !is.na(regex) ) {
		if(length(grep("[\\.\\*\\[\\(\\+\\|^]+", regex) )<1){ # no grep found: add some anchors for safety
			regex = paste("^", regex, "[0-9]*$", sep=""); # anchor to the start of the string
			anchored = T
			if(verbose == T) {
				message("note: anchored regex to beginning of string and allowed only numeric follow\n");
			}
		}else{
			anchored=F
		}
		theLabels = grep(regex, theLabels, perl = F, value = T) # return more detail
		if(length(theLabels) == 0){
			msg = "Found no matching labels!\n"
			if(anchored == T){
				msg = paste0(msg, "note: anchored regex to beginning of string and allowed only numeric follow:\"", regex, "\"")
			}
			if(umx_is_MxModel(inputTarget)){
				msg = paste0(msg, "\nUse umxGetParameters(", deparse(substitute(inputTarget)), ") to see all parameters in the model")
			}else{
				msg = paste0(msg, "\nUse umxGetParameters() without a pattern to see all parameters in the model")
			}
			stop(msg);
		}
	}
	return(theLabels)
}

#' umxEquate
#'
#' Equate parameters by setting one or more labels (the slave set) equal
#' to the labels in a master set. By setting two parameters to have the 
#' \code{\link{umxLabel}}, they are then forced to have the same value.
#' In addition to matching labels, you may wish to learn about set the 
#' label of a slave parameter to the "square bracket" address of the
#' master, i.e. model.matrix[r,c].
#' 
#' Tip: To find labels of free parameters use \code{\link{umxGetParameters}} with free = T
#' 
#' @param model   An \code{\link{mxModel}} within which to equate parameters
#' @param master  A list of "master" labels to which slave labels will be equated
#' @param slave   A list of slave labels which will be updated to match master labels, thus equating the parameters
#' @param free    Should parameter(s) initally be free? (default = TRUE)
#' @param verbose Whether to give verbose feedback (default = TRUE)
#' @param name    name for the returned model (optional: Leave empty to leave name unchanged)
#' @return - \code{\link{mxModel}}
#' @export
#' @family umx model updating and comparison
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = F, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = T, setValues = T)
#' m2 = umxEquate(m1, master = "G_to_x1", slave = "G_to_x2", name = "Equate x1 and x2 loadings")
#' m2 = mxRun(m2) # have to run the model again...
#' umxCompare(m1, m2) # not good :-)
#' umxSummary(m1, m2) # not good :-)
umxEquate <- function(model, master, slave, free = TRUE, verbose = TRUE, name = NULL) {	
	# add the T|F|NA list stuff to handle free = c(T|F|NA)
	if(!umx_is_RAM(model)){
		message("ERROR in umxEquate: model must be a model, you gave me a ", class(model)[1])
		message("A usage example is umxEquate(model, master=\"a_to_b\", slave=\"a_to_c\", name=\"model2\") # equate paths a->b and a->c, in a new model called \"model2\"")
		stop()
	}
	if(length(grep("[\\^\\.\\*\\[\\(\\+\\|]+", master) ) < 1){ # no grep found: add some anchors
		master = paste0("^", master, "$"); # anchor to the start of the string
		slave  = paste0("^", slave,  "$");
		if(verbose == T){
			cat("note: matching whole label\n");
		}
	}
	masterLabels = names(omxGetParameters(model, indep = FALSE, free = free))
	masterLabels = masterLabels[which(!is.na(masterLabels) )]      # exclude NAs
	masterLabels = grep(master, masterLabels, perl = F, value = T)
	# return(masterLabels)
	slaveLabels = names(omxGetParameters(model, indep = F, free = free))
	slaveLabels = slaveLabels[which(!is.na(slaveLabels))] # exclude NAs
	slaveLabels = grep(slave, slaveLabels, perl = F, value = T)
	if( length(slaveLabels) != length(masterLabels)) {
		print(list(masterLabels = masterLabels, slaveLabels = slaveLabels))
		stop("ERROR in umxEquate: master and slave labels not the same length!")
	}
	if( length(slaveLabels)==0 ) {
		legal = names(omxGetParameters(model, indep=FALSE, free=free))
		legal = legal[which(!is.na(legal))]
		message("Labels available in model are: ", paste(legal, ", "))
		stop("ERROR in umxEquate: no matching labels found!")
	}
	print(list(masterLabels = masterLabels, slaveLabels = slaveLabels))
	model = omxSetParameters(model = model, labels = slaveLabels, newlabels = masterLabels, name = name)
	model = omxAssignFirstParameters(model, indep = F)
	return(model)
}

#' umxDrop1
#'
#' Drops each free parameter (selected via regex), returning an \code{\link{mxCompare}}
#' table comparing the effects. A great way to quickly determine which of several 
#' parameters can be dropped without excessive cost
#'
#' @param model An \code{\link{mxModel}} to drop parameters from 
#' @param regex A string to select parameters to drop. leave empty to try all.
#' This is regular expression enabled. i.e., "^a_" will drop parameters beginning with "a_"
#' @param maxP The threshold for returning values (defaults to p==1 - all values)
#' @return a table of model comparisons
#' @export
#' @seealso - \code{\link{grep}}, \code{\link{umxLabel}}, \code{\link{umxRun}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' \dontrun{
#' umxDrop1(fit3) # try dropping each free parameters (default)  
#' # drop "a_r1c1" and "a_r1c2" and see which matters more.
#' umxDrop1(model, regex="a_r1c1|a_r1c2")
#' }
umxDrop1 <- function(model, regex = NULL, maxP = 1) {
	if(is.null(regex)) {
		toDrop = umxGetParameters(model, free = T)
	} else if (length(regex) > 1) {
		toDrop = regex
	} else {
		toDrop = grep(regex, umxGetParameters(model, free = T), value = T, ignore.case = T)
	}
	message("Will drop each of ", length(toDrop), " parameters: ", paste(toDrop, collapse = ", "), ".\nThis might take some time...")
	out = list(rep(NA, length(toDrop)))
	for(i in seq_along(toDrop)){
		tryCatch({
			message("item ", i, " of ", length(toDrop))
        	out[i] = umxReRun(model, name = paste0("drop_", toDrop[i]), regex = toDrop[i])
		}, warning = function(w) {
			message("Warning incurred trying to drop ", toDrop[i])
			message(w)
		}, error = function(e) {
			message("Error occurred trying to drop ", toDrop[i])
			message(e)
		})
	}
	out = data.frame(umxCompare(model, out))
	out[out=="NA"] = NA
	suppressWarnings({
		out$p   = as.numeric(out$p) 
		out$AIC = as.numeric(out$AIC)
	})
	n_row = dim(out)[1] # 2 9
	sortedOrder = order(out$p[2:n_row])+1
	out[2:n_row, ] <- out[sortedOrder, ]
	good_rows = out$p < maxP
	good_rows[1] = T
	message(sum(good_rows)-1, "of ", length(out$p)-1, " items were beneath your p-threshold of ", maxP)
	return(out[good_rows, ])
}

#' umxAdd1
#'
#' Add each of a set of paths you provide to the model, returning a table of theire effect on fit
#'
#' @param model an \code{\link{mxModel}} to alter
#' @param pathList1 a list of variables to generate a set of paths
#' @param pathList2 an optional second list: IF set paths will be from pathList1 to members of this list
#' @param arrows Make paths with one or two arrows
#' @param maxP The threshold for returning values (defaults to p==1 - all values)
#' @return a table of fit changes
#' @export
#' @seealso - \code{\link{umxDrop1}}, \code{\link{mxModel}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' \dontrun{
#' model = umxAdd1(model)
#' }
umxAdd1 <- function(model, pathList1 = NULL, pathList2 = NULL, arrows = 2, maxP = 1) {
	# DONE: RAM paths
	# TODO add non-RAM
	if ( is.null(model@output) ) stop("Provided model hasn't been run: use mxRun(model) first")
	# stop if there is no output
	if ( length(model@output) < 1 ) stop("Provided model has no output. use mxRun() first!")

	if(arrows == 2){
		if(!is.null(pathList2)){
			a = xmuMakeTwoHeadedPathsFromPathList(pathList1)
			b = xmuMakeTwoHeadedPathsFromPathList(pathList2)
			a_to_b = xmuMakeTwoHeadedPathsFromPathList(c(pathList1, pathList2))
			toAdd = a_to_b[!(a_to_b %in% c(a,b))]
		}else{
			if(is.null(pathList1)){
				stop("best to set pathList1!")
				# toAdd = umxGetParameters(model, free = F)
			} else {
				toAdd = xmuMakeTwoHeadedPathsFromPathList(pathList1)
			}
		}
	} else if(arrows == 1){
		if(is.null(pathList2)){
			stop("pathList2 must not be empty for arrows = 1: it forms the target of each path")
		} else {
			toAdd = xmuMakeOneHeadedPathsFromPathList(pathList1, pathList2)
		}
	}else{
		stop("You idiot :-) : arrows must be either 1 or 2, you tried", arrows)
	}
	# TODO fix count? or drop giving it?
	message("You gave me ", length(pathList1), "source variables. I made ", length(toAdd), " paths from these.")

	# Just keep the ones that are not already free... (if any)
	toAdd2 = toAdd[toAdd %in% umxGetParameters(model, free = F)]
	if(length(toAdd2) == 0){
		if(length(toAdd[toAdd %in% umxGetParameters(model, free = NA)] == 0)){
			message("I couldn't find any of those paths in this model.",
				"The most common cause of this error is submitting the wrong model")
			message("You asked for: ", paste(toAdd, collapse=", "))
		}else{
			message("I found (at least some) of those paths in this model, but they were already free")
			message("You asked for: ", paste(toAdd, collapse=", "))
		}		
		stop()
	}else{
		toAdd = toAdd2
	}
	message("Of these ", length(toAdd), " were currently fixed, and I will try adding them")
	message(paste(toAdd, collapse = ", "))

	message("This might take some time...")
	flush.console()
	# out = data.frame(Base = "test", ep = 1, AIC = 1.0, p = 1.0); 
	row1Cols = c("Base", "ep", "AIC", "p")
	out = data.frame(umxCompare(model)[1, row1Cols])
	for(i in seq_along(toAdd)){
		# model = fit1 ; toAdd = c("x2_with_x1"); i=1
		message("item ", i, " of ", length(toAdd))
		tmp = omxSetParameters(model, labels = toAdd[i], free = T, value = .01, name = paste0("add_", toAdd[i]))
		tmp = mxRun(tmp)
		mxc = umxCompare(tmp, model)
		newRow = mxc[2, row1Cols]
		newRow$AIC = mxc[1, "AIC"]
		out = rbind(out, newRow)
	}

	out[out=="NA"] = NA
	out$p   = round(as.numeric(out$p), 3)
	out$AIC = round(as.numeric(out$AIC), 3)
	out <- out[order(out$p),]
	if(maxP==1){
		return(out)
	} else {
		good_rows = out$p < maxP
		message(sum(good_rows, na.rm = T), "of ", length(out$p), " items were beneath your p-threshold of ", maxP)
		message(sum(is.na(good_rows)), " was/were NA")
		good_rows[is.na(good_rows)] = T
		return(out[good_rows, ])
	}
}

# ===============
# = RAM Helpers =
# ===============

# =========================
# = path-oriented helpers =
# =========================

#' umxLatent
#'
#' Helper to ease the creation of latent variables including formative and reflective variables (see below)
#' For formative variables, the manifests define (form) the latent.
#' This function takes care of intercorrelating manifests for formatives, and fixing variances correctly
#' 
#' The following figures show how a reflective and a formative variable look as path diagrams:
#' \figure{reflective.png}
#' formative\figure{formative.png}
#'
#' @param latent the name of the latent variable (string)
#' @param formedBy the list of variables forming this latent
#' @param forms the list of variables which this latent forms (leave blank for formedBy)
#' @param data the dataframe being used in this model
#' @param type = \"exogenous|endogenous\"
#' @param model.name = NULL
#' @param labelSuffix a string to add to the end of each label
#' @param verbose = T
#' @param endogenous This is now deprecated. use type= \"exogenous|endogenous\"
#' @return - path list
#' @export
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' \dontrun{
#' library(OpenMx)
#' library(umx)
#' data(demoOneFactor)
#' latents = c("G")
#' manifests = names(demoOneFactor) # x1-5
#' theData = cov(demoOneFactor)
#' m1 = mxModel("reflective", type = "RAM",
#'	manifestVars = manifests,
#'	latentVars   = latents,
#'	# Factor loadings
#'	umxLatent("G", forms = manifests, type = "exogenous", data = theData),
#'	mxData(theData, type = "cov", numObs = nrow(demoOneFactor))
#' )
#' m1 = umxRun(m1, setValues = T, setLabels = T); umxSummary(m1, show="std")
#' umxPlot(m1)
#' 
#' m2 = mxModel("formative", type = "RAM",
#'	manifestVars = manifests,
#'	latentVars   = latents,
#'	# Factor loadings
#'	umxLatent("G", formedBy = manifests, data = theData),
#'	mxData(theData, type = "cov", numObs = nrow(demoOneFactor))
#' )
#' m2 = umxRun(m2, setValues = T, setLabels = T); umxSummary(m2, show="std")
#' umxPlot(m2)
#' }
umxLatent <- function(latent = NULL, formedBy = NULL, forms = NULL, data = NULL, type = NULL,  model.name = NULL, labelSuffix = "", verbose = T, endogenous = "deprecated") {
	# Purpose: make a latent variable formed/or formed by some manifests
	# Use: umxLatent(latent = NA, formedBy = manifestsOrigin, data = df)
	# TODO: delete manifestVariance
	# Check both forms and formedBy are not defined
	if(!endogenous == "deprecated"){
		if(endogenous){
			stop("Error in mxLatent: Use of endogenous=T is deprecated. Remove it and replace with type = \"endogenous\"") 
		} else {
			stop("Error in mxLatent: Use of endogenous=F is deprecated. Remove it and replace with type = \"exogenous\"") 
		}
	}
	if(is.null(latent)) { stop("Error in mxLatent: you have to provide the name of the latent variable you want to create") }
	if( is.null(formedBy) &&  is.null(forms)) { stop("Error in mxLatent: Must define one of forms or formedBy") }
	if(!is.null(formedBy) && !is.null(forms)) { stop("Error in mxLatent: Only one of forms or formedBy can be set") }
	if(is.null(data)) { stop("Error in mxLatent: you have to provide the data that will be used in the model") }
	# ==========================================================
	# = NB: If any vars are ordinal, a call to umxMakeThresholdsMatrices
	# = will fix the mean and variance of ordinal vars to 0 and 1
	# ==========================================================
	# Warning("If you use this with a dataframe containing ordinal variables, don't forget to call umxAutoThreshRAMObjective(df)")
	isCov = umx_is_cov(data, boolean = T)
	if( any(!is.null(forms))) {
		manifests <- forms
	}else{
		manifests <- formedBy
	}
	if(isCov) {
		variances = diag(data[manifests, manifests])
	} else {
		manifestOrdVars = umxIsOrdinalVar(data[,manifests])
		if(any(manifestOrdVars)) {
			means         = rep(0, times = length(manifests))
			variances     = rep(1, times = length(manifests))
			contMeans     = colMeans(data[,manifests[!manifestOrdVars], drop = F], na.rm = T)
			contVariances = diag(cov(data[,manifests[!manifestOrdVars], drop = F], use = "complete"))
			if( any(!is.null(forms)) ) {
				contVariances = contVariances * .1 # hopefully residuals are modest
			}
			means[!manifestOrdVars] = contMeans				
			variances[!manifestOrdVars] = contVariances				
		}else{
			if(verbose){
				message("No ordinal variables")
			}
			means     = colMeans(data[, manifests], na.rm = T)
			variances = diag(cov(data[, manifests], use = "complete"))
		}
	}

	if( any(!is.null(forms)) ) {
		# Handle forms case
		# p1 = Residual variance on manifests
		# p2 = Fix latent variance @ 1
		# p3 = Add paths from latent to manifests
		p1 = mxPath(from = manifests, arrows = 2, free = T, values = variances)
		if(is.null(type)){ stop("Error in mxLatent: You must set type to either exogenous or endogenous when creating a latent variable with an outgoing path") }
		if(type == "endogenous"){
			# Free latent variance so it can do more than just redirect what comes in
			if(verbose){
				message(paste("latent '", latent, "' is free (treated as a source of variance)", sep=""))
			}
			p2 = mxPath(from=latent, connect="single", arrows=2, free=T, values=.5)
		} else {
			# fix variance at 1 - no inputs
			if(verbose){
				message(paste("latent '", latent, "' has variance fixed @ 1"))
			}
			p2 = mxPath(from=latent, connect="single", arrows=2, free=F, values=1)
		}
		p3 = mxPath(from = latent, to = manifests, connect = "single", free = T, values = variances)
		if(isCov) {
			# Nothing to do: covariance data don't need means...
			paths = list(p1, p2, p3)
		}else{
			# Add means: fix latent mean @0, and add freely estimated means to manifests
			p4 = mxPath(from = "one", to = latent   , arrows = 1, free = F, values = 0)
			p5 = mxPath(from = "one", to = manifests, arrows = 1, free = T, values = means)
			paths = list(p1, p2, p3, p4, p5)
		}
	} else {
		# Handle formedBy case
		# Add paths from manifests to the latent
		p1 = mxPath(from = manifests, to = latent, connect = "single", free = T, values = umxValues(.6, n=manifests)) 
		# In general, manifest variance should be left free...
		# TODO If the data were correlations... we can inspect for that, and fix the variance to 1
		p2 = mxPath(from = manifests, connect = "single", arrows = 2, free = T, values = variances)
		# Allow manifests to intercorrelate
		p3 = mxPath(from = manifests, connect = "unique.bivariate", arrows = 2, free = T, values = umxValues(.3, n = manifests))
		if(isCov) {
			paths = list(p1, p2, p3)
		}else{
			# Fix latent mean at 0, and freely estimate manifest means
			p4 = mxPath(from="one", to=latent   , free = F, values = 0)
			p5 = mxPath(from="one", to=manifests, free = T, values = means)
			paths = list(p1, p2, p3, p4, p5)
		}
	}
	if(!is.null(model.name)) {
		m1 <- mxModel(model.name, type="RAM", manifestVars = manifests, latentVars = latent, paths)
		if(isCov){
			m1 <- mxModel(m1, mxData(cov(df), type="cov", numObs = 100))
			message("\n\nIMPORTANT: you need to set numObs in the mxData() statement\n\n\n")
		} else {
			if(any(manifestOrdVars)){
				m1 <- mxModel(m1, umxThresholdRAMObjective(data, deviationBased = T, droplevels = T, verbose = T))
			} else {
				m1 <- mxModel(m1, mxData(data, type = "raw"))
			}
		}
		return(m1)
	} else {
		return(paths)
	}
	# TODO	shift this to a test file
	# readMeasures = paste("test", 1:3, sep="")
	# bad usages
	# mxLatent("Read") # no too defined
	# mxLatent("Read", forms=manifestsRead, formedBy=manifestsRead) #both defined
	# m1 = mxLatent("Read", formedBy = manifestsRead, model.name="base"); umxPlot(m1, std=F, dotFilename="name")
	# m2 = mxLatent("Read", forms = manifestsRead, as.model="base"); 
	# m2 <- mxModel(m2, mxData(cov(df), type="cov", numObs=100))
	# umxPlot(m2, std=F, dotFilename="name")
	# mxLatent("Read", forms = manifestsRead)
}

umxConnect <- function(x) {
	# TODO handle endogenous
}

umxSingleIndicators <- function(manifests, data, labelSuffix = "", verbose = T){
	# use case
	# mxSingleIndicators(manifests, data)
	if( nrow(data) == ncol(data) & all(data[lower.tri(data)] == t(data)[lower.tri(t(data))]) ) {
		isCov = T
		if(verbose){
			message("treating data as cov")
		}
	} else {
		isCov = F
		if(verbose){
			message("treating data as raw")
		}
	}
	if(isCov){
		variances = diag(data[manifests,manifests])
		# Add variance to the single manfests
		p1 = mxPath(from=manifests, arrows=2, value=variances)
		return(p1)
	} else {
		manifestOrdVars = umxIsOrdinalVar(data[,manifests])
		if(any(manifestOrdVars)){
			means         = rep(0, times=length(manifests))
			variances     = rep(1, times=length(manifests))
			contMeans     = colMeans(data[,manifests[!manifestOrdVars], drop = F], na.rm=T)
			contVariances = diag(cov(data[,manifests[!manifestOrdVars], drop = F], use="complete"))
			means[!manifestOrdVars] = contMeans				
			variances[!manifestOrdVars] = contVariances				
		}else{
			means     = colMeans(data[,manifests], na.rm = T)
			variances = diag(cov(data[,manifests], use = "complete"))
		}
		# Add variance to the single manfests
		p1 = mxPath(from = manifests, arrows = 2, value = variances) # labels = mxLabel(manifests, suffix = paste0("unique", labelSuffix))
		# Add means for the single manfests
		p2 = mxPath(from="one", to=manifests, values=means) # labels = mxLabel("one", manifests, suffix = labelSuffix)
		return(list(p1, p2))
	}
}

# ===========================
# = matrix-oriented helpers =
# ===========================

# ===================
# = Ordinal helpers =
# ===================

#' umxThresholdRAMObjective
#'
#' umxThresholdRAMObjective can set the means to 0 and variance of the latents to 1, 
#' and build an appropriate thresholds matrix
#' 
#' It uses \code{\link{umx_is_ordinal}} and \code{\link{umxMakeThresholdMatrix}} as helpers
#'
#' @param df Dataframe to make a threshold matrix for
#' @param deviationBased whether to use the deviation system to ensure order thresholds (default = T)
#' @param droplevels whether to also drop unused levels (default = T)
#' @param verbose whether to say what the function is doing (default = F)
#' @return - \code{\link{mxModel}}
#' @export
#' @family advanced umx helpers
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' \dontrun{
#' model = umxThresholdRAMObjective(model)
#' }
umxThresholdRAMObjective <- function(df, deviationBased = T, droplevels = T, verbose = F) {
	# TODO: means = zero & VAR = 1 for ordinal variables
	# (This is a nice place to check, as we have the df present...)
	if(!any(umx_is_ordinal(df))){
		stop("No ordinal variables in dataframe: no need to call umxThresholdRAMObjective")
	} 
	pt1 = mxPath(from = "one", to = umxIsOrdinalVar(df,names = T), connect="single", free=F, values = 0)
	pt2 = mxPath(from = umxIsOrdinalVar(df, names = T), connect = "single", arrows = 2, free = F, values = 1)
	return(list(pt1, pt2, umxMakeThresholdMatrix(df, deviationBased = T, droplevels = T, verbose = F)))
}

#' umxMakeThresholdMatrix
#'
#' The purpose of this function is to generate an mxRAMObjective. It is used by \code{\link{umxThresholdRAMObjective}}.
#' You likely want that, not this.
#'
#' @param df Dataframe for which to make a threshold matrix.
#' @param deviationBased whether to use the deviation system to ensure order thresholds (default = TRUE)
#' @param droplevels whether to also drop unused levels (default = TRUE)
#' @param verbose whether to say what the function is doing (default = FALSE)
#' @return - \code{\link{mxModel}}
#' @family advanced umx helpers
#' @export
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' umxMakeThresholdMatrix(mtcars, verbose = T)
umxMakeThresholdMatrix <- function(df, deviationBased = T, droplevels = F, verbose = F) {
	# mxRAMObjective(A = "A", S="S", F="F", M="M", thresholds = "thresh"), mxData(df, type="raw")
	# use case:  
	# TODO: Let the user know if there are any levels dropped...
	if(droplevels){
		df = droplevels(df)
	}
	if(deviationBased){
		return(xmuMakeDeviationThresholdsMatrices(df, droplevels, verbose))
	} else {
		return(xmuMakeThresholdsMatrices(df, droplevels, verbose))
	}
}


# ===========
# = Utility =
# ===========
#' umxJiggle
#'
#' umxJiggle takes values in a matrix and jiggles them
#'
#' @param matrixIn an \code{\link{mxMatrix}} to jiggle the values of
#' @param mean the mean value to add to each value
#' @param sd the sd of the jiggle noise
#' @param dontTouch A value, which, if found, will be left as-is (defaults to 0)
#' @return - \code{\link{mxMatrix}}
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxValues}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @export
#' @examples
#' \dontrun{
#' mat1 = umxJiggle(mat1)
#' }

umxJiggle <- function(matrixIn, mean = 0, sd = .1, dontTouch = 0) {
	mask = (matrixIn != dontTouch);
	newValues = mask;
	matrixIn[mask == TRUE] = matrixIn[mask == TRUE] + rnorm(length(mask[mask == TRUE]), mean = mean, sd = sd);
	return (matrixIn);
}

umxCheck <- function(fit1){
	# are all the manifests in paths?
	# do the manifests have residuals?
	if(any(duplicated(fit1@manifestVars))){
		stop(paste("manifestVars contains duplicates:", duplicated(fit1@manifestVars)))
	}
	if(length(fit1@latentVars) == 0){
		# Check none are duplicates, none in manifests
		if(any(duplicated(fit1@latentVars))){
			stop(paste("latentVars contains duplicates:", duplicated(fit1@latentVars)))
		}
		if(any(duplicated(c(fit1@manifestVars,fit1@latentVars)))){
			stop(
				paste("manifest and latent lists contain clashing names:", duplicated(c(fit1@manifestVars,fit1@latentVars)))
			)
		}
	}
	# Check manifests in dataframe
}

# ====================
# = Parallel Helpers =
# ====================

eddie_AddCIbyNumber <- function(model, labelRegex = "") {
	# eddie_AddCIbyNumber(model, labelRegex="[ace][1-9]")
	args     = commandArgs(trailingOnly=TRUE)
	CInumber = as.numeric(args[1]); # get the 1st argument from the cmdline arguments (this is called from a script)
	CIlist   = umxGetLabels(model ,regex= "[ace][0-9]", verbose=F)
	thisCI   = CIlist[CInumber]
	model    = mxModel(model, mxCI(thisCI) )
	return (model)
}

#' Helper functions for OpenMx
#'
#' umx allows you to more easily build, run, modify, and report models using OpenMx
#' with code. The core functions are linked below (see "family")
#'
#' All the functions have explanatory examples, so use the help, even if you think it won't help :-)
#' Have a look, for example at \code{\link{umxRun}}
#' There's also a working example below and in demo(umx)
#' When I have a vignette, it will be: vignette("umx", package = "umx")
#' 
#' umx lives on github at present \url{http://github.com/tbates/umx}
#' 
#' There is a helpful blog at \url{http://tbates.github.io}
#' 
#' To install:
#' install.packages("devtools")
#' library("devtools")
#' 
#' install_github("tbates/umx")
#' library("umx")
#' 
#' @aliases umx-package
#' @family umx core functions
#' @references - \url{"http://www.github.com/tbates/umx"}
#' 
#' @examples
#' require("OpenMx")
#' require("umx")
#' data(demoOneFactor)
#' latents = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM",
#' 	manifestVars = manifests,
#' 	latentVars  = latents,
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents  , arrows = 2, free = F, values = 1),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = nrow(demoOneFactor))
#' )
#' 
#' omxGetParameters(m1) # nb: By default, paths have no labels, and starts of 0
#' 
#' # With \code{link{umxLabel}}, you can easily add informative and predictable labels to each free path (works with matrix style as well!)
#' # and use \code{link{umxValues}}, to set sensible guesses for start values...
#' m1 = umxLabel(m1)  
#' m1 = umxValues(m1)  
#' 
#' # Re-run omxGetParameters...
#' omxGetParameters(m1) # Wow! Now your model has informative labels, & better starts
#' 
#' # umxRun the model (calculates saturated models for raw data, & repeats if the model is not code green)
#' m1 = umxRun(m1, setLabels = T, setValues = T) # not needed given we've done this above. But you can see how umxRun enables 1-line setup and run
#' 
#' # Let's get some journal-ready fit information
#' 
#' umxSummary(m1) 
#' umxSummary(m1, show = "std") #also display parameter estimates 
#' 
#' # ==================
#' # = Model updating =
#' # ==================
#' # Can we set the loading of X5 on G to zero?
#' m2 = omxSetParameters(m1, labels = "G_to_x1", values = 0, free = F, name = "no_effect_of_g_on_X5")
#' m2 = mxRun(m2)
#' # Compare the two models
#' umxCompare(m1, m2)
#' 
#' # Use umxReRun to do the same thing in 1-line
#' m2 = umxReRun(m1, "G_to_x1", name = "no_effect_of_g_on_X5", comparison = T)
#' 
#' # =================================
#' # = Get some Confidence intervals =
#' # =================================
#' 
#' confint(m1, run = TRUE) # lots more to learn about ?confint.MxModel
#' 
#' # And make a Figure it dot format!
#' # If you have installed GraphViz, the next command will open it for you to see!
#' 
#' # umxPlot(m1, std = T)
#' # Run this instead if you don't have GraphViz
#' umxPlot(m1, std = T, dotFilename = NA)
#' @docType package
#' @name umx
NULL

umxParallel <- function(model, what) {
	stop("umxparallel not implemented")
	# TODO make it easy to turn parallel on and off
	# omxDetectCores() # cores available
	# getOption('mxOptions')$"Number of Threads" # cores used by OpenMx
	# mxOption(model= yourModel, key="Number of Threads", value= (omxDetectCores() - 1))
}

#' umxPath: Flexible specification of sem paths
#'
#' This function returns a standard mxPath, but gives new options for specifying the path. In addition to the normal
#' from and to, it adds specialised parameters for variances (var), two headed paths (with) and means (mean).
#' There are also verbs for fixing values: "fixedAt" and "fixFirst"
#' Finally, it also allows sem-style "A->B" string specification.
#'
#' @description The goal of this function is to enable quck to write, quick to read, and flexible paths for RAM models in OpenMx.
#' 
#' The new key "with" means you no-longer need set arrows = 2 on covariances. So you can say:
#'    \code{mxPath(A, with = B)} instead of \code{mxPath(from = A, to = B, arrows = 2)}.
#' 
#' Dpecify a variance with \code{mxPath(var = A)} (equivalent to \code{mxPath(from = A, to = A, arrows = 2)}).
#' 
#' To specify a mean, you say \code{mxPath(mean = A)}, which is equivalent to \code{mxPath(from = "one", to = A)}.
#' 
#' To fix a patha at a value, you can say \code{mxPath(var = A, fixedAt = 1)} instead of to \code{mxPath(from = A, to = A, arrows = 2, free = F, values = 1)}.
#' 
#' Setting up a latent trait, you can fix the loading of the first path with \code{mxPath(A, to = c(B,C,D), fixFirst = T)} instead of 
#' \code{mxPath(from = A, to = c(B,C,D), free = c(F, T, T), values = c(1, .5, .4))}.
#' 
#' Finally, you can use the John Fox "sem" package style notation, i.e., "A -> B".
#' If you want to add multiple paths that way, separate them with a semi-colon or a return (see examples below.)
#' 
#' 
#' @param from either a source variable e.g "A" or c("A","B"), OR a sem-style path description, e.g. "A-> B" or "C <> B"
#' @param to one or more target variables for one-headed paths, e.g "A" or c("A","B") 
#' @param with same as "to = vars, arrows = 2". nb: from, to= and var=  must be left empty (their default)
#' @param var equivalent to setting "from = vars, arrows = 2". nb: from, to, and with must be left empty (their default)
#' @param cov equivalent to setting "from = X, to = Y, arrows = 2". nb: from, to, and with must be left empty (their default)
#' @param unique.bivariate equivalent to setting "connect = "unique.bivariate", arrows = 2". nb: from, to, and with must be left empty (their default)
#' @param means equivalent to "from = 'one', to = x. nb: from, to, with and var must be left empty (their default).
#' @param v1m0 variance of 1 and mean of zero in one call.
#' @param fixedAt Equivalent to setting "free = FALSE, values = x" nb: free and values must be left empty (their default)
#' @param firstAt first value is fixed at 1 (free is ignored: warning if not a single TRUE)
#' @param connect as in mxPath - nb: Only used when using from and to
#' @param arrows as in mxPath - nb: Only used when using from and to
#' @param free whether the value is free to be optimised
#' @param values default value list
#' @param labels labels for each path
#' @param lbound lower bounds for each path value
#' @param ubound upper bounds for each path value
#' @return - \code{\link{mxPath}}
#' @export
#' @family umx build functions
#' @seealso - \code{\link{umxLabel}}, \code{\link{mxMatrix}}, \code{\link{umxStart}}
#' @references - \url{https://github.com/tbates/umx}, \url{tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' require(OpenMx)
#' # Some examples of paths with umxPath
#' umxPath("A", to = "B")
#' umxPath("A", to = "B", fixedAt = 1) 
#' umxPath("A", to = LETTERS[2:4], firstAt = 1) # Same as "free = FALSE, values = 1"
#' umxPath("A", with = "B") # using with: same as "to = B, arrows = 2"
#' umxPath("A", with = "B", fixedAt = .5)
#' umxPath("A", with = "B", firstAt = 1)
#' umxPath("A", with = c("B","C"), fixedAt = 1)
#' umxPath(var = "A") # Give a variance to A
#' umxPath(var = "A", fixedAt = 1)
#' umxPath(var = LETTERS[1:5], fixedAt = 1)
#' umxPath(cov = c("A", "B")) # Covariance A <-> B
#' umxPath(means = c("A","B")) # Create a means model for A: from = "one", to = "A"
#' umxPath(means = c("A","B"), values = c(pi,exp(1)))
# These are not yet implemented
#' umxPath("A <-> B") # same path as above using a string
#' umxPath("A -> B") # one-headed arrow with string syntax
#' umxPath("A <> B; A <-- B") # This is ok too
#' umxPath("A -> B; B>C; C --> D") # two paths. space doesn't matter, hyphens don't matter
#' umxPath("A -> manifests") # manifests is a reserved word, as is latents. Allows string syntax to use the manifestVars variable
#' # A worked example
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	umxPath(latents, to = manifests),
#' 	# umxPath("G -> manifests"),
#' 	mxPath(var = manifests),
#' 	mxPath(var = latents, fixedAt = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = T, setValues = T)
#' umxSummary(m1, show = "std")
umxPath <- function(from = NULL, to = NULL, with = NULL, var = NULL, cov = NULL, unique.bivariate = NULL, means = NULL, v1m0 = NULL, fixedAt = NULL, firstAt = NULL, connect = "single", arrows = 1, free = TRUE, values = NA, labels = NA, lbound = NA, ubound = NA) {
	if(!is.null(from)){
		if(length(from) > 1){
			isSEMstyle = grepl("[<>]", x = from[1])	
		} else {
			isSEMstyle = grepl("[<>]", x = from)				
		}
		if(isSEMstyle){
			stop("sem-style string syntax not yet implemented. In the mean time, try the other features, like with, var, means = , fixedAt = , fixFirst = ")
			if("from contains an arrow"){
				# parse into paths
			} else {
				if(!is.null(with)){
					to = with
					arrows = 2
					connect = "single"
				} else {
					to = to
					arrows = 1
					connect = "single"
				}
			}	
			a = "A ->B;A<-B; A>B; A --> B
			A<->B"
			# remove newlines, replacing with ;
			allOneLine = gsub("\n+", ";", a, ignore.case = TRUE)
			# regularizedArrows = gsub("[ \t]?^<-?>[ \t]?", "->", allOneLine, ignore.case = TRUE)
			# regularizedArrows = gsub("[ \t]?-?>[ \t]?", "<-", regularizedArrows, ignore.case = TRUE)

			# TODO remove duplicate ; 
			pathList = umx_explode(";", allOneLine)
			for (aPath in pathList) {
				if(length(umx_explode("<->", aPath))==3){
					# bivariate
					parts = umx_explode("<->", aPath)
					mxpath(from = umx_trim(parts[1]))
				} else if(length(umx_explode("->", aPath))==3){
					# from to
				} else if(length(umx_explode("<-", aPath))==3){
					# to from
				}else{
					# bad line
				}
			}
			umx_explode("", a)
		}
	}
	n = 0

	for (i in list(with, cov, var, means, unique.bivariate, v1m0)) {
		if(!is.null(i)){ n = n + 1}
	}
	if(n > 1){
		stop("At most one of with, cov, var, unique.bivariate, v1m0, and means can be use at one time")
	} else if(n == 0){
		# check that from is set?
		if(is.null(from)){
			stop("You must set at least 'from'")
		}	
	} else {
		# n = 1
	}

	if(!is.null(v1m0)){
		a = mxPath(from = v1m0, arrows = 2, free = FALSE, values = 1)
		b = mxPath(from = "one", to = v1m0, free = FALSE, values = 0)
		return(list(a,b))
	}

	if(!is.null(with)){
		# ===============
		# = Handle with =
		# ===============
		if(is.null(from)){
			stop("To use with, you must set 'from = ' also")
		} else {
			from = from
			to   = with
			arrows = 2
			connect = "single"
		}
	} else if(!is.null(cov)){
		# ==============
		# = Handle cov =
		# ==============
		if(!is.null(from) | !is.null(to)){
			stop("To use 'cov = ', 'from' and 'to' should be empty")
		} else if (length(cov) != 2){
			stop("cov must consist of two and only two variables.\n",
			"If you want to covary more variables, use: 'unique.bivariate =' \n",
			"or else use 'from =', 'to=', and 'connect = \"unique.bivariate\"'\n",
			"If you want to set variances for a list of variables, use 'var = c(\"X\")'")
		} else {
			from   = cov[1]
			to     = cov[2]
			arrows = 2
			connect = "single"
		}
	} else if(!is.null(var)){
		# ==============
		# = handle var =
		# ==============
		if(!is.null(from) | !is.null(to)){
			stop("To use 'var = ', 'from' and 'to' should be empty")
		} else {
			from   = var
			to     = var
			arrows = 2
			connect = "single"
		}
	} else if(!is.null(means)){
		# ================
		# = Handle means =
		# ================
		if(!is.null(from) | !is.null(to)){
			stop("To use means, from and to should be empty.",
			"Just say 'means = c(\"X\",\"Y\").'")
		} else {
			from   = "one"
			to     = means
			arrows = 1
			connect = "single"
		}
	} else if(!is.null(unique.bivariate)){
		# ===========================
		# = Handle unique.bivariate =
		# ===========================
		if(!is.null(from) | !is.null(to)){
			stop("To use unique.bivariate, 'from=' and 'to=' should be empty.\n",
			"Just say 'unique.bivariate = c(\"X\",\"Y\").'")
		} else {
			from    = unique.bivariate
			to    = NA
			arrows  = 2
			connect = "unique.bivariate"
		}
	} else {
		if(is.null(from) & is.null(to)){
			stop("You don't seem to have requested any paths.\n",
			"see help(umxPath) for all the possibilities")
		} else {
			# assume it is from to
			from    = from
			to      = to
			arrows  = arrows
			connect = "single"
		}
	}
	# ==================================
	# = From and to will be set now... =
	# ==================================

	# ===============================
	# =  handle fixedAt and firstAt =
	# ===============================
	if(!is.null(fixedAt) & !is.null(firstAt)){
		stop("At most one of fixedAt and firstAt can be set: You seem to have tried to set both at once.")
	}

	# Handle firstAt
	if(!is.null(firstAt)){
		if(length(from) > 1){
			# TODO think about this
			stop("It's not wise to use firstAt with multiple from sources. I'd like to think about this before implementing it..")
		} else {
			free    = rep(TRUE, length(to))
			free[1] = FALSE
			values    = rep(NA, length(to))
			values[1] = firstAt
		}
	}	
	# Handle fixedAt
	if(!is.null(fixedAt)){
		free = FALSE
		values = fixedAt
	}

	# TODO check incoming value of connect
	# if(!connect == "single"){
	# 	message("Connect should be single, it was:", connect)
	# }	
	mxPath(from = from, to = to, connect = connect, arrows = arrows, free = free, values = values, labels = labels, lbound = lbound, ubound = ubound)
}


umxMatrix <- function(type = "Full", rc= NULL, fixedAt = NULL,
	nrow = NA, ncol = NA, free = FALSE, 
	values = NA, labels = "auto", 
	lbound = NA, ubound = NA, 
	byrow = getOption('mxByrow'), 
	dimnames = NA, name = NA) {
	if(!is.null(rc)){
		if(!length(rc)==2){
			stop("rc must be a collection of two numbers, i.e., rc = c(3,4)")
		} else if(any(!is.na(list(nrow, ncol)))){
			stop("if you set rc, nrow and ncol must be left empty")		
		}
		nrow = rc[1]
		ncol = rc[2]
	} else {
		alt.expr
	}

	if(!is.null(fixedAt)){
		if(!is.na(values)){
			stop("if you set fixedAt, values must be left empty")
		}
		free   = FALSE
		values = fixedAt
	}

	if(labels == "auto"){
		m = mxMatrix(type = "Full", nrow = nrow, ncol = ncol, free = free, values = values, labels = NA, lbound = lbound, ubound = ubound, byrow = getOption('mxByrow'), dimnames = dimnames, name = name)
		m = umxLabel(m)
	} else{
		m = mxMatrix(type = "Full", nrow = nrow, ncol = ncol, free = free, values = values, labels = labels, lbound = lbound, ubound = ubound, byrow = getOption('mxByrow'), dimnames = dimnames, name = name)
	}
	return(m)
}

# nCov = 3
# umxMatrix(name = "covsT1", rc = c(1, nCov), fixedAt = , labels = data.cov.T1)
# umxMatrix(name = "covsT2", rc = c(1, nCov), values = 1)

# devtools::document("~/bin/umx")     ; devtools::install("~/bin/umx");


#' umx_add_std
#'
#' Add algebras to a RAM model so that it can report CIs on standardized paths.
#' If you just want standardized paths, or SEs on these, call \link[OpenMx]{mxStandardizeRAMpaths}
#'
#' @param model an \code{\link{mxModel}} to add standardization algebra to
#' @param addCIs whether to also add the mxCI calls to use these standardization matrices.
#' @return - \code{\link{mxModel}}
#' @export
#' @family umx build functions
#' @references - \url{https://github.com/tbates/umx}, \url{tbates.github.io}
#' @examples
#' \dontrun{
#' model = umx_add_std(model)
#' }
umx_add_std <- function(model, addCIs = TRUE) {
	if(!umx_is_RAM(model)){
		stop("umx_add_std only works on RAM models at present")
	}
	nVar = dim(model@matrices$A)[[1]]
	dnames = dimnames(a1@matrices$S@labels)
	model <- mxModel(model,
		mxMatrix( name = "I", type = "Iden", nrow = nVar, ncol = nVar),
		mxMatrix( name = "unitColumn", type = "Unit", nrow = nVar, ncol = 1),
		mxAlgebra(name = "IA", solve(I - A)),
		mxAlgebra(name = "expCov", IA %&% S),
		# mxAlgebra(name = "invSDs", vec2diag(1/sqrt(diag2vec(expCov))) ),
		mxAlgebra(name = "invSDs", vec2diag(unitColumn/sqrt(diag2vec(expCov))) ),
		mxAlgebra(name = "stdA"  , invSDs %*% A %*% solve(invSDs), dimnames = dnames),
		mxAlgebra(name = "stdS"  , invSDs %*% S %*% invSDs, dimnames = dnames)
	)
	if(addCIs){
		freeA = umx_get_bracket_addresses(model@matrices$A, free= TRUE, newName = "stdA")
		freeS = umx_get_bracket_addresses(model@matrices$S, free= TRUE, newName = "stdS")
		model = mxModel(model, mxCI(c(freeS, freeA)) )
	}
	return(model)
}
