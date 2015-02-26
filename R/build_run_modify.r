options('mxCondenseMatrixSlots'= FALSE)
# svn update; make clean; make install; osascript -e 'quit app "R"'; open /Applications/R.app; osascript -e 'tell app "R"'; osascript -e 'tell app "R" to cmd "library(OpenMx)” '

# osascript -e 'quit app "R"'; open /Applications/R.app; osascript -e 'tell app "R" to cmd "library(devtools)" '; osascript -e 'tell app "R" to cmd "document(\"~/bin/umx\"); install(\"~/bin/umx\"); ?umx" '

# library(devtools)
# setwd("~/bin/umx");
# devtools::document("~/bin/umx"); devtools::install("~/bin/umx");
# devtools::check_doc("~/bin/umx")
# devtools::run_examples("~/bin/umx")
# devtools::build("~/bin/umx")
# devtools::load_all("~/bin/umx")
# devtools::show_news("~/bin/umx")
# source('http://openmx.psyc.virginia.edu/getOpenMxBeta.R')
# system(paste("open", shQuote("/Users/tim/bin/umx/R/misc_and_utility.r")))
# install.packages("OpenMx", "~/Dropbox/shared folders/OpenMx_binaries/OpenMx2.0.1/3.1.1-snowleopard/OpenMx_2.0.1-4133.tgz")
# create()
# add_travis();
# update_version();
# news();
# create_README()

# devtools::document("~/bin/umx.twin"); devtools::install("~/bin/umx.twin"); 

# https://r-forge.r-project.org/project/admin/?group_id=1745
# http://r-pkgs.had.co.nz/
# http://adv-r.had.co.nz/Philosophy.html
# https://github.com/hadley/devtools

# ===============================
# = Highlevel models (ACE, GxE) =
# ===============================

# ssh -R 52698:localhost:52698 tbates@eddie.ecdf.ed.ac.uk
# or just eddie
# https://www.wiki.ed.ac.uk/display/ecdfwiki/Steps+to+run+an+OpenMP+job+on+Eddie

# setUpPB <- function(updateAuth = NULL) {
# 	if (doesnt exist .rpushbullet.json){
# 		if(is.null(updateAuth)){
# 			stop(prompt for key with instructions)
# 		} else {
# 			# create file and store updateAuth
# 		}
# 	}else{
# 		read file
# 		if (!is.null(updateAuth){
# 			# print the old key to console in case this is a mistake
# 			# save updateAuth
# 		}
# 	}
# 	} if (is.null(updateAuth)){
# 		# prompt for key with instructions
# 	} else {
# 		# have key
# 		# populate file.
# 		# fix nicknames?
# 		# use nicknames?
# 	}
# }

.onAttach <- function(libname, pkgname){
    packageStartupMessage("For an overview type '?umx'")
}

# =====================================================================================================
# = Create a class for ACE models so we can subclass plot and umxSummary to handle them automagically =
# =====================================================================================================
setClass("MxModel.ACE", contains = "MxModel")


#' umxRAM
#'
#' Making it as simple as possible to create a RAM model, without doing things invisible to the user.
#' 
#' @details Like mxModel, you list the theoretical causal paths. Unlike mxModel:
#' \enumerate{
#' \item{You can request creation of error variances using \code{endog.variances = TRUE} }
#' \item{You can request creation of variances for exogenous variables (using \code{exog.variances = TRUE})}
#' \item{For identification, you can request either \code{fix = "latents"} or \code{fix = "firstLoadings"} 
#' to fix either the variance of latents or their first factor loading at 1.}
#' }
#' Additional conveniences: 
#' \enumerate{
#' \item{type defaults to "RAM"}
#' \item{You don't need to list manifestVars (they are assumed to map onto names in the \code{mxData})}
#' \item{Any variables you mention that are not found in mxData are assumed to be latents}
#' }
#' 
#' @param name friendly name for the model
#' @param data the data for the model. Can be an \code{\link{mxData}} or a data.frame
#' @param ... A list of mxPath, umxPath, or mxThreshold objects
#' @param exog.variances If TRUE, free variance parameters are added for exogenous variables that lack them (the default is FALSE).
#' @param endog.variances If TRUE, free error-variance parameters are added for any endogenous variables that lack them (default is FALSE).
#' @param fix Whether to fix latent or first paths to 1. Options are: c("none", "latents", "firstLoadings") (defaults to "none")
#' @param latentVars Latents you want in your model (defaults to NULL, in which case any variable not in the data is assumed to be a latent variable)
#' @param remove_unused_manifests Whether to remove variables in the data to which no path makes reference (defaults to TRUE)
#' @param setValues Whether to try and guess good start values (Defults to TRUE, set them)
#' @param independent Whether the model is independent (default = NA)
#' @return - \code{\link{mxModel}}
#' @export
#' @family Model Building Functions
#' @references - \url{https://github.com/tbates/umx}, \url{http://tbates.github.io}
#' @examples
#' # umxRAM is like ggplot2::qplot(), you give the data in a data =  parameter
#' # A common error is to include data in the main list,
#' # a bit like saying lm(y~x + df) instead of lm(y~x, data=dd)...
#' # nb: unlike mxModel, umxRAM needs data at build time.
#' 
#' # 1. For convenience, list up the manifests you will be using
#' selVars = c("mpg", "wt", "disp")
#' 
#' # 2. Create an mxData object
#' myCov = mxData(cov(mtcars[,selVars]), type = "cov", numObs = nrow(mtcars) )
#' 
#' # 3. Write the model and paths (see ?umxPath for LOTS more neat options)
#' m1 = umxRAM("tim", data = myCov,
#' 	umxPath(c("wt", "disp"), to = "mpg"),
#' 	umxPath(cov = c("wt", "disp")),
#' 	umxPath(var = c("wt", "disp", "mpg"))
#' )
#' 
#' # 4. Run the model
#' m1 = mxRun(m1)
#' 
#' # 5. Print a nice summary 
#' umxSummary(m1, show = "std")
#' 
#' \dontrun{
#' # 6. Draw a nice path diagram (needs Graphviz)
#' plot(m1)
#' }
umxRAM <- function(name, data = NULL, ..., exog.variances = FALSE, endog.variances = FALSE, fix = c("none", "latents", "firstLoadings"), latentVars = NULL, setValues = TRUE, independent = NA, remove_unused_manifests = TRUE) {
	fix = umx_default_option(fix, c("none", "latents", "firstLoadings"), check = TRUE)
	dot.items = list(...) # grab all the dot items: mxPaths, etc...
	if(!length(dot.items) > 0){
	}
	if(is.null(data)){
		stop("umxRAM needs some mxData. You set this like in lm(), with data = mxData().\nDid you perhaps just add the mxData along with the paths?")
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
		independent = independent,
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
		stop("fix is not supported any longer: switch to umxPath with firstAt and fixedAt to be more up front about model content\n",
		"or use m1 = umx_fix_first_loadings(m1), or m1 = umx_fix_latents(m1)")
		# TODO turn this off, now that umxPath makes it easy...
		# Fix latents or first paths
		if(fix == "latents"){
			m1 = umx_fix_latents(m1)
		} else if(fix == "firstLoadings"){
			# add free variance to latents not in the fixed list?
			m1 = umx_fix_first_loadings(m1)
		}else{
			stop("Unknown option for fix", fix)
		}
	}

	if(isRaw){
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
	if(setValues){
		m1 = umxValues(m1, onlyTouchZeros = TRUE)
	}
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
#' @param weightCov Whether to use cov.wt matrices or FIML default = FALSE, i.e., FIML
#' @param width An option to widen or narrow the window from its default (of 1)
#' @param target A user-selected list of moderator values to test (default = NULL = explore the full range)
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
#' mod = "age"         # The name of the moderator column in the dataset
#' selDVs = c("bmi1", "bmi2") # The DV for twin 1 and twin 2
#' data(twinData) # Dataset of Australian twins, built into OpenMx
#' # The twinData consist of two cohorts. First we label them
#' # TODO: Q for OpenMx team: can I add a cohort column to this dataset?
#' twinData$cohort = 1; twinData$cohort[twinData$zyg %in% 6:10] = 2
#' twinData$zyg[twinData$cohort == 2] = twinData$zyg[twinData$cohort == 2]-5
#' # And set a plain-English label
#' labList = c("MZFF", "MZMM", "DZFF", "DZMM", "DZOS")
#' twinData$ZYG = factor(twinData$zyg, levels = 1:5, labels = labList)
#' # The model also assumes two groups: MZ and DZ. Moderator can't be missing
#' # Delete missing moderator rows
#' twinData = twinData[!is.na(twinData[mod]),]
#' mzData = subset(twinData, ZYG == "MZFF", c(selDVs, mod))
#' dzData = subset(twinData, ZYG == "DZFF", c(selDVs, mod))
#' 
#' # ========================
#' # = 2. Run the analyses! =
#' # ========================
#' # Run with FIML (default) uses all information
#' umxGxE_window(selDVs = selDVs, moderator = mod, mzData = mzData, dzData = dzData);
#' 
#' # Run creating weighted covariance matrices (excludes missing data)
#' umxGxE_window(selDVs = selDVs, moderator = mod, mzData = mzData, dzData = dzData, 
#' 		weightCov = TRUE); 
#' 
#' # Run and plot for specified windows (in this case just 1927)
#' umxGxE_window(selDVs = selDVs, moderator = mod, mzData = mzData, dzData = dzData, 
#' 		target = 40, plotWindow = TRUE)
#' 
#' @family Twin Modeling Functions
#' @references - Hildebrandt, A., Wilhelm, O, & Robitzsch, A. (2009)
#' Complementary and competing factor analytic approaches for the investigation 
#' of measurement invariance. \emph{Review of Psychology}, \bold{16}, 87--107. 
#' 
#' Briley, D, Bates, T.C., Harden, K., Tucker-Drob, E. (2015)
#' Of mice and men: Local SEM in gene environment analysis. \emph{Behavior Genetics}.

umxGxE_window <- function(selDVs = NULL, moderator = NULL, mzData = mzData, dzData = dzData, weightCov = FALSE, target = NULL, width = 1, plotWindow = FALSE, return = c("estimates","last_model")) {
	# TODO want to allow missing moderator?
	# Check moderator is set and exists in mzData and dzData
	if(is.null(moderator)){
		stop("Moderator must be set to the name of the moderator column, e.g, moderator = \"birth_year\"")
	}
	# Check DVs exists in mzData and dzData (and nothing else apart from the moderator)
	umx_check_names(c(selDVs, moderator), data = mzData, die = TRUE, no_others = TRUE)
	umx_check_names(c(selDVs, moderator), data = dzData, die = TRUE, no_others = TRUE)

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
	# bind the MZ nd DZ data into one frame so we can work with it repeatedly over weight iterations
	allData = rbind(mzData, dzData)

	# Create range of moderator values to iterate over (using the incoming moderator variable name)
	modVar  = allData[, moderator]
	if(any(is.na(modVar))){		
		stop("Moderator \"", moderator, "\" contains ", length(modVar[is.na(modVar)]), "NAs. This is not currently supported.\n",
			"NA found on rows", paste(which(is.na(modVar)), collapse = ", "), " of the combined data."
		)
	}

	if(!is.null(target)){
		if(target < min(modVar)) {
			stop("specifiedTarget is below the range in moderator. min(modVar) was ", min(modVar))
		} else if(target > max(modVar)){
			stop("specifiedTarget is above the range in moderator. max(modVar) was ", max(modVar))
		} else {
			targetLevels = target
		}
	} else {
		# by default, run across each integer value of the moderator
		targetLevels = seq(min(modVar), max(modVar))
	}

	numPairs     = nrow(allData)
	moderatorSD  = sd(modVar, na.rm = TRUE)
	bw           = 2 * numPairs^(-.2) * moderatorSD *  width # -.2 == -1/5 

	ACE = c("A", "C", "E")
	tmp = rep(NA, length(targetLevels))
	out = data.frame(modLevel = targetLevels, Astd = tmp, Cstd = tmp, Estd = tmp, A = tmp, C = tmp, E = tmp)
	n   = 1
	for (i in targetLevels) {
		# i = targetLevels[1]
		message("mod = ", i)
		zx = (modVar - i)/bw
		k = (1 / (2 * pi)^.5) * exp((-(zx)^2) / 2)
		# ===========================================================
		# = Insert the weights variable into dataframes as "weight" =
		# ===========================================================
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
	# Squaring paths to produce variances
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
#' @param suffix The suffix for twin 1 and twin 2, often "_T" (defaults to NULL) With this, you can
#' omit suffixes from names in SelDV, i.e., just "dep" not c("dep_T1", "dep_T2")
#' @param dzAr The DZ genetic correlation (defaults to .5, set to .25 for dominance model)
#' @param dzCr The DZ genetic correlation (defaults to 1,  vary to examine assortative mating)
#' @param addStd Whether to add the algebras to compute a std model (defaults to TRUE)
#' @param addCI Whether to add intervals to compute CIs (defaults to TRUE)
#' @param numObsDZ = Number of DZ twins: Set this if you input covariance data
#' @param numObsMZ = Number of MZ twins: Set this if you input covariance data
#' @param boundDiag = whether to bound the diagonal of the a, c, and e matrices
#' @param weightVar = If provided, a vector objective will be used to weight the data. (default = NULL) 
#' @param equateMeans Whether to equate the means across twins (defaults to TRUE)
#' @param bVector whether to compute row-wise likelihoods (defaults to FALSE)
#' @return - \code{\link{mxModel}} of subclass mxModel.ACE
#' @export
#' @family Twin Modeling Functions
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' # Height, weight, and BMI data from Australian twins. 
#' # The total sample has been subdivided into a young cohort, aged 18-30 years,
#' # and an older cohort aged 31 and above.
#' # Cohort 1 Zygosity is coded as follows: 
#' # 1 == MZ females 2 == MZ males 3 == DZ females 4 == DZ males 5 == DZ opposite sex pairs
#' # tip: ?twinData to learn more about this data set
#' require(OpenMx)
#' require(umx.twin)
#' data(twinData)
#' names(twinData)
#' # "fam", "age", "zyg", "part", "wt1", "wt2", "ht1", "ht2", "htwt1", "htwt2", "bmi1", "bmi2"
#' 
#' # Set the zygosity to a factor
#' labList = c("MZFF", "MZMM", "DZFF", "DZMM", "DZOS")
#' twinData$ZYG = factor(twinData$zyg, levels = 1:5, labels = labList)
#' # Pick the variable the zygosity to a factor
#' selDVs = c("bmi1", "bmi2") # nb: Can also give base name, (i.e., "bmi") AND set suffix.
#' # the function will then make the varnames for each twin using 
#' # c(paste0("bmi", suffix, 1), paste0("bmi", suffix, 2))
#' mzData <- subset(twinData, ZYG == "MZFF", selDVs)
#' dzData <- subset(twinData, ZYG == "DZFF", selDVs)
#' m1 = umxACE(selDVs = selDVs, dzData = dzData, mzData = mzData)
#' m1 = umxRun(m1)
#' umxSummaryACE(m1)
#' umxSummary(m1)
#' \dontrun{
#' plot(m1)
#' }
#' # ADE model (DZ correlation set to .25)
#' m2 = umxACE("ADE", selDVs = selDVs, dzData = dzData, mzData = mzData, dzCr = .25)
#' m2 = umxRun(m2)
#' mxCompare(m2, m1) # ADE is better
#' umxSummary(m2) # nb: though this is ADE, it's labeled ACE
#' 
#' 
#' # Ordinal example
#' require(OpenMx)
#' data(twinData)
#' labList = c("MZFF", "MZMM", "DZFF", "DZMM", "DZOS")
#' twinData$zyg = factor(twinData$zyg, levels = 1:5, labels = labList)
#' # Cut weight to to form ordinal obesity
#' ordDVs = c("obese1", "obese2")
#' selDVs = c("obese")
#' obesityLevels = c('normal', 'overweight', 'obese')
#' cutPoints <- quantile(twinData[, "bmi1"], probs = c(.5, .2), na.rm = TRUE)
#' twinData$obese1 <- cut(twinData$bmi1, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' twinData$obese2 <- cut(twinData$bmi2, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' # Make the ordinal variables into mxFactors (ensure ordered is TRUE, and require levels)
#' twinData[, ordDVs] <- mxFactor(twinData[, ordDVs], levels = obesityLevels)
#' mzData <- subset(twinData, zyg == "MZFF", umx_paste_names(selDVs, "", 1:2))
#' dzData <- subset(twinData, zyg == "DZFF", umx_paste_names(selDVs, "", 1:2))
#' str(mzData)
#' m1 = umxACE(selDVs = selDVs, dzData = dzData, mzData = mzData, suffix = '')
#' m1 = mxRun(m1)
#' umxSummary(m1)
#' \dontrun{
#' # plot(m1)
#' }
#' 
#' # Bivariate continuous and ordinal example (assumes examples above have been run)
#' selDVs = c("wt", "obese")
#' mzData <- subset(twinData, zyg == "MZFF", umx_paste_names(selDVs, "", 1:2))
#' dzData <- subset(twinData, zyg == "DZFF", umx_paste_names(selDVs, "", 1:2))
#' m1 = umxACE(selDVs = selDVs, dzData = dzData, mzData = mzData, suffix = '')
#' m1 = umxRun(m1)
#' umxSummary(m1)
#' 
#' 
#' # Mixed continuous and binary example
#' require(OpenMx)
#' data(twinData)
#' labList = c("MZFF", "MZMM", "DZFF", "DZMM", "DZOS")
#' twinData$zyg = factor(twinData$zyg, levels = 1:5, labels = labList)
#' # Cut to form category of 20% obese subjects
#' cutPoints <- quantile(twinData[, "bmi1"], probs = .2, na.rm = TRUE)
#' obesityLevels = c('normal', 'obese')
#' twinData$obese1 <- cut(twinData$bmi1, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' twinData$obese2 <- cut(twinData$bmi2, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' # Make the ordinal variables into mxFactors (ensure ordered is TRUE, and require levels)
#' ordDVs = c("obese1", "obese2")
#' twinData[, ordDVs] <- mxFactor(twinData[, ordDVs], levels = obesityLevels)
#' selDVs = c("wt", "obese")
#' mzData <- subset(twinData, zyg == "MZFF", umx_paste_names(selDVs, "", 1:2))
#' dzData <- subset(twinData, zyg == "DZFF", umx_paste_names(selDVs, "", 1:2))
#' str(dzData)
#' m1 = umxACE(selDVs = selDVs, dzData = dzData, mzData = mzData, suffix = '')
#' m1 = umxRun(m1)
#' umxSummary(m1)
#' # ===================================
#' # Example with covariance data only =
#' # ===================================
#' selDVs = c("wt1", "wt2")
#' dz = cov(dzData[, selDVs], use = "complete")
#' mz = cov(mzData[, selDVs], use = "complete")
#' m1 = umxACE(selDVs=selDVs, dzData=dz, mzData=mz, numObsDZ=nrow(dzData), numObsMZ=nrow(mzData))
#' m1 = mxRun(m1)
#' summary(m1)
#' \dontrun{
#' plot(m1)
#' }
umxACE <- function(name = "ACE", selDVs, dzData, mzData, suffix = NULL, dzAr = .5, dzCr = 1, addStd = TRUE, addCI = TRUE, numObsDZ = NULL, numObsMZ = NULL, boundDiag = NULL, weightVar = NULL, equateMeans = TRUE, bVector = FALSE) {
	nSib = 2 # number of siblings in a twin pair
	# look for name conflicts
	badNames = umx_grep(selDVs, grepString = "^[ACDEacde][0-9]*$")
	if(!identical(character(0), badNames)){
		stop("The data contain variables that look like parts of the a, c, e model, i.e., a1 is illegal.\n",
		"BadNames included: ", omxQuotes(badNames) )
	}

	if(!is.null(suffix)){
		if(length(suffix) > 1){
			stop("suffix should be just one word, like '_T'. I will add 1 and 2 afterwards... \n",
			"i.e., you have to name your variables 'obese_T1' and 'obese_T2' etc.")
		}
		selDVs = umx_paste_names(selDVs, suffix, 1:2)
	}
	umx_check_names(selDVs, mzData)
	umx_check_names(selDVs, dzData)
	# message("selDVs: ", omxQuotes(selDVs))
	nVar = length(selDVs)/nSib; # number of dependent variables ** per INDIVIDUAL ( so times-2 for a family)**

	dataType = umx_is_cov(dzData, boolean = FALSE)
	# compute numbers of ordinal and binary variables
	if(dataType == "raw"){
		if(!all(is.null(c(numObsMZ, numObsDZ)))){
			stop("You should not be setting numObsMZ or numObsDZ with ", omxQuotes(dataType), " data...")
		}
		isFactor = umx_is_ordered(mzData[, selDVs])                      # T/F list of factor columns
		isOrd    = umx_is_ordered(mzData[, selDVs], ordinal.only = TRUE) # T/F list of ordinal (excluding binary)
		isBin    = umx_is_ordered(mzData[, selDVs], binary.only  = TRUE) # T/F list of binary columns
		nFactors = sum(isFactor)
		nOrdVars = sum(isOrd) # total number of ordinal columns
		nBinVars = sum(isBin) # total number of binary columns

		factorVarNames = names(mzData)[isFactor]
		ordVarNames    = names(mzData)[isOrd]
		binVarNames    = names(mzData)[isBin]
		contVarNames   = names(mzData)[!isFactor]
	} else {
		# summary data
		isFactor = isOrd = isBin = c()
		nFactors = nOrdVars = nBinVars = 0
		factorVarNames = ordVarNames = binVarNames = contVarNames = c()
	}
	if(nFactors > 0 & is.null(suffix)){
		stop("Please set suffix.\n",
		"Why: You have included ordinal or binary variables. I need to know which variables are for twin 1 and which for twin2.\n",
		"The way I do this is enforcing some naming rules. For example, if you have 2 variables:\n",
		" obesity and depression called: 'obesity_T1', 'dep_T1', 'obesity_T2' and 'dep_T2', you should call umxACE with:\n",
		"selDVs = c('obesity','dep'), suffix = '_T' \n",
		"suffix is just one word, appearing in all variables (e.g. '_T').\n",
		"This is assumed to be followed by '1' '2' etc...")
	}

	used = selDVs
	if(!is.null(weightVar)){
		used = c(used,weightVar)
	}
	# Drop unused columns from mz and dzData
	mzData = mzData[, used]
	dzData = dzData[, used]

	if(dataType == "raw") {
		if(!is.null(weightVar)){
			# weight variable provided: check it exists in each frame
			if(!umx_check_names(weightVar, data = mzData, die = FALSE) | !umx_check_names(weightVar, data = dzData, die = FALSE)){
				stop("The weight variable must be included in the mzData and dzData",
					 " frames passed into umxACE when \"weightVar\" is specified",
					 "\n mzData contained:", paste(names(mzData), collapse = ", "),
					 "\n and dzData contain:", paste(names(dzData), collapse = ", "),
					 "\nbut I was looking for ", weightVar, " as the moderator."
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

		# ===============================
		# = Notes: Ordinal requires:    =
		# ===============================
		# 1. Means of binary vars fixedAt 0
		# 2. A+C+E for binary vars is constrained to 1 
		# 3. First 2 thresholds fixed for ordinal mxFactors

		# ===========================
		# = Add means matrix to top =
		# ===========================
		# Figure out ace starts while we are here
		# varStarts will be used to fill a, c, and e
		# mxMatrix(name = "a", type = "Lower", nrow = nVar, ncol = nVar, free = TRUE, values = varStarts, byrow = TRUE)
		varStarts = umx_cov_diag(mzData[, selDVs[1:nVar], drop = FALSE], ordVar = 1, use = "pairwise.complete.obs")
		if(nVar == 1){
			varStarts = varStarts/3
		} else {
			varStarts = t(chol(diag(varStarts/3))) # divide variance up equally, and set to Cholesky form.
		}
		varStarts = matrix(varStarts, nVar, nVar)

		# Mean starts (used across all raw solutions
		obsMZmeans = umx_means(mzData[, selDVs], ordVar = 0, na.rm = TRUE)
		meanDimNames = list("means", selDVs)		
		# smarter but not guaranteed
		# a_val = e_val = t(chol(xmu_cov_factor(mzData, use = "pair"))) * .6
		# c_val = t(chol(cov(mzData, use = "pair"))) * .1
		if(nFactors == 0) {
			# =======================================================
			# = Handle all continuous case                          =
			# =======================================================
			message("All variables continuous")
			meansMatrix = mxMatrix(name = "expMean", "Full" , nrow = 1, ncol = (nVar * nSib), free = TRUE, values = obsMZmeans, dimnames = meanDimNames)
			top = mxModel("top", umxLabel(meansMatrix))
			MZ  = mxModel("MZ" , mxExpectationNormal("top.expCovMZ", "top.expMean"), mxFitFunctionML(vector = bVector), mxData(mzData, type = "raw") )
			DZ  = mxModel("DZ" , mxExpectationNormal("top.expCovDZ", "top.expMean"), mxFitFunctionML(vector = bVector), mxData(dzData, type = "raw") )
		} else if(sum(isBin) == 0){
			# =======================================================
			# = Handle some 1 or more ordinal variables (no binary) =
			# =======================================================
			# Means: all free, start cont at the measured value, ord @0
			meansMatrix  = mxMatrix(name = "expMean", "Full" , nrow = 1, ncol = (nVar * nSib), free = TRUE, values = obsMZmeans, dimnames = meanDimNames)
			# Thresholds
			# for better guessing with low-freq cells
			allData = rbind(mzData, dzData)
			# threshMat may be a three item list of matrices and algebra
			threshMat = umxThresholdMatrix(allData, suffixes = paste0(suffix, 1:2), verbose = FALSE)
			# return(threshMat)
			mzExpect  = mxExpectationNormal("top.expCovMZ", "top.expMean", thresholds = "top.threshMat")
			dzExpect  = mxExpectationNormal("top.expCovDZ", "top.expMean", thresholds = "top.threshMat")			
			top = mxModel("top", umxLabel(meansMatrix), threshMat)
			MZ  = mxModel("MZ", mzExpect, mxFitFunctionML(vector = bVector), mxData(mzData, type = "raw") )
			DZ  = mxModel("DZ", dzExpect, mxFitFunctionML(vector = bVector), mxData(dzData, type = "raw") )
		} else if(sum(isBin) > 0){
			# =======================================================
			# = Handle case of at least 1 binary variable           =
			# =======================================================

			message("umxACE found ", sum(isBin)/nSib, " pairs of binary variables:", omxQuotes(binVarNames))
			message("\nI am fixing the latent means and variances of these variables to 0 and 1")
			# ===========================================================================
			# = Means: bin fixed, others free, start cont at the measured value, ord @0 =
			# ===========================================================================
			# Fill with zeros: default for ordinals and binary...
			meansFree = (!isBin) # fix the binary variables at zero
			meansMatrix = mxMatrix(name = "expMean", "Full" , nrow = 1, ncol = nVar*nSib, free = meansFree, values = obsMZmeans, dimnames = meanDimNames)

			# = Thresholds =
			# For better guessing with low-freq cells
			allData = rbind(mzData, dzData)
			# threshMat may be a three item list of matrices and algebra
			threshMat = umxThresholdMatrix(allData, suffixes = paste0(suffix, 1:2), verbose = FALSE)
			mzExpect  = mxExpectationNormal("top.expCovMZ", "top.expMean", thresholds = "top.threshMat")
			dzExpect  = mxExpectationNormal("top.expCovDZ", "top.expMean", thresholds = "top.threshMat")

			top = mxModel("top", umxLabel(meansMatrix), threshMat)
			MZ  = mxModel("MZ", mzExpect, mxFitFunctionML(vector = bVector), mxData(mzData, type = "raw") )
			DZ  = mxModel("DZ", dzExpect, mxFitFunctionML(vector = bVector), mxData(dzData, type = "raw") )

			# ===================================
			# = Constrain Ordinal variance @ 1  =
			# ===================================
			# Algebra to pick out the ord vars
			# TODO check this way of using twin 1 to pick where the bin vars are is robust...
			the_bin_cols = which(isBin)[1:nVar] # columns in which the bin vars appear for twin 1, i.e., c(1,3,5,7)
			binBracketLabels = paste0("Vtot[", the_bin_cols, ",", the_bin_cols, "]")

			top = mxModel(top,
				# Algebra to compute total variances and standard deviations
				mxAlgebra(name = "Vtot", A + C+ E), # Total variance (redundant but is OK)
				mxMatrix(name  = "binLabels"  , "Full", nrow = (nBinVars/nSib), ncol = 1, labels = binBracketLabels),
				mxMatrix(name  = "Unit_nBinx1", "Unit", nrow = (nBinVars/nSib), ncol = 1),
				mxConstraint(name = "constrain_Bin_var_to_1", binLabels == Unit_nBinx1)
			)
		} else {
			stop("You appear to have something other than I expected in terms of binary, ordinal and continuous variable mix")
		}
		# nb: means not yet equated across twins
	} else if(dataType %in% c("cov", "cor")){
		if(!is.null(weightVar)){
			stop("You can't set weightVar when you give cov data - use cov.wt to create weighted cov matrices, or pass in raw data")
		}
		umx_check(!is.null(numObsMZ), "stop", paste0("You must set numObsMZ with ", dataType, " data"))
		umx_check(!is.null(numObsDZ), "stop", paste0("You must set numObsDZ with ", dataType, " data"))
		# TODO should keep this just as mzData?
		het_mz = umx_reorder(mzData, selDVs)		
		het_dz = umx_reorder(dzData, selDVs)
		varStarts = diag(het_mz)
		if(nVar == 1){
			varStarts = varStarts/3
		} else {
			varStarts = t(chol(diag(varStarts/3))) # divide variance up equally, and set to Cholesky form.
		}
		varStarts = matrix(varStarts, nVar, nVar)

		top = mxModel("top")
		MZ = mxModel("MZ", 
			mxExpectationNormal("top.expCovMZ"), 
			mxFitFunctionML(), 
			mxData(het_mz, type = "cov", numObs = numObsMZ)
		)
		
		DZ = mxModel("DZ",
			mxExpectationNormal("top.expCovDZ"),
			mxFitFunctionML(),
			mxData(het_dz, type = "cov", numObs = numObsDZ)
		)
	} else {
		stop("Datatype \"", dataType, "\" not understood. Must be one of raw, cov, or cor")
	}
	message("treating data as ", dataType)

	# Finish building top
	top = mxModel(top,
		# "top" defines the algebra of the twin model, which MZ and DZ slave off of
		# NB: top already has the means model and thresholds matrix added if necessary  - see above
		# Additive, Common, and Unique environmental paths
		umxLabel(mxMatrix(name = "a", type = "Lower", nrow = nVar, ncol = nVar, free = T, values = varStarts, byrow = T)),
		umxLabel(mxMatrix(name = "c", type = "Lower", nrow = nVar, ncol = nVar, free = T, values = varStarts, byrow = T)),
		umxLabel(mxMatrix(name = "e", type = "Lower", nrow = nVar, ncol = nVar, free = T, values = varStarts, byrow = T)),  
		
		mxMatrix(name = "dzAr", type = "Full", 1, 1, free = FALSE, values = dzAr),
		mxMatrix(name = "dzCr", type = "Full", 1, 1, free = FALSE, values = dzCr),
		# Multiply by each path coefficient by its inverse to get variance component
		# Quadratic multiplication to add common_loadings
		mxAlgebra(a %*% t(a), name = "A"), # additive genetic variance
		mxAlgebra(c %*% t(c), name = "C"), # common environmental variance
		mxAlgebra(e %*% t(e), name = "E"), # unique environmental variance
		mxAlgebra(A+C+E     , name = "ACE"),
		mxAlgebra(A+C       , name = "AC" ),
		mxAlgebra( (dzAr %x% A) + (dzCr %x% C),name = "hAC"),
		mxAlgebra(rbind (cbind(ACE, AC),
		                 cbind(AC , ACE)), dimnames = list(selDVs, selDVs), name = "expCovMZ"),
		mxAlgebra(rbind (cbind(ACE, hAC),
		                 cbind(hAC, ACE)), dimnames = list(selDVs, selDVs), name = "expCovDZ")
	)

	if(!bVector){
		model = mxModel(name, MZ, DZ, top,
			mxFitFunctionMultigroup(c("MZ", "DZ"))
		)
	} else {
		# bVector is TRUE
		# To weight objective functions in OpenMx, you specify a container model that applies the weights
		# m1 is the model with no weights, but with "vector = TRUE" option added to the FIML objective.
		# This option makes FIML return individual likelihoods for each row of the data (rather than a single -2LL value for the model)
		# You then optimize weighted versions of these likelihoods by building additional models containing 
		# weight data and an algebra that multiplies the likelihoods from the first model by the weight vector
		model = mxModel(name, MZ, DZ, top,
			mxModel("MZw", mzWeightMatrix,
				mxAlgebra(-2 * sum(mzWeightMatrix * log(MZ.objective) ), name = "mzWeightedCov"),
				mxFitFunctionAlgebra("mzWeightedCov")
			),
			mxModel("DZw", dzWeightMatrix,
				mxAlgebra(-2 * sum(dzWeightMatrix * log(DZ.objective) ), name = "dzWeightedCov"),
				mxFitFunctionAlgebra("dzWeightedCov")
			),
			mxFitFunctionMultigroup(c("MZw", "DZw"))
		)
	}
	if(!is.null(boundDiag)){
		diag(model@submodels$top@matrices$a@lbound) = boundDiag
		diag(model@submodels$top@matrices$c@lbound) = boundDiag
		diag(model@submodels$top@matrices$e@lbound) = boundDiag
	}
	if(addStd){
		newTop = mxModel(model@submodels$top,
			mxMatrix(name  = "I", "Iden", nVar, nVar), # nVar Identity matrix
			mxAlgebra(name = "Vtot", A + C+ E),       # Total variance
			# TODO test that these are identical in all cases
			# mxAlgebra(vec2diag(1/sqrt(diag2vec(Vtot))), name = "SD"), # Total variance
			mxAlgebra(name = "SD", solve(sqrt(I * Vtot))), # Total variance
			mxAlgebra(name = "a_std", SD %*% a), # standardized a
			mxAlgebra(name = "c_std", SD %*% c), # standardized c
			mxAlgebra(name = "e_std", SD %*% e)  # standardized e
		)
		model = mxModel(model, newTop)
		if(addCI){
			model = mxModel(model, mxCI(c('top.a_std', 'top.c_std', 'top.e_std')))
		}
	}
	# Equate means for twin1 and twin 2 by matching labels in the first and second halves of the means labels matrix
	if(equateMeans & (dataType == "raw")){
		model = omxSetParameters(model,
		  labels    = paste0("expMean_r1c", (nVar + 1):(nVar * 2)), # c("expMean14", "expMean15", "expMean16"),
		  newlabels = paste0("expMean_r1c", 1:nVar)             # c("expMean11", "expMean12", "expMean13")
		)
	}
	# Just trundle through and make sure values with the same label have the same start value... means for instance.
	model = omxAssignFirstParameters(model)
	model = as(model, "MxModel.ACE") # change type so that plot() (actually plot.MxModel.ACE) will work.
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
#' @family Model Building Functions
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' mxEval(S, m1) # default variances are 0
#' m1 = umxValues(m1)
#' mxEval(S, m1) # plausible variances
#' umx_print(mxEval(S,m1), 3, zero.print = ".") # plausible variances
#' umxValues(14, sd = 1, n = 10) # Return vector of length 10, with mean 14 and sd 1
#' # todo: handle complex guided matrix value starts...
umxValues <- function(obj = NA, sd = NA, n = 1, onlyTouchZeros = FALSE) {
	if(is.numeric(obj) ) {
		# Use obj as the mean, return a list of length n, with sd = sd
		return(xmu_start_value_list(mean = obj, sd = sd, n = n))
	} else if (umx_is_MxMatrix(obj) ) {
		message("Let's put values into a matrix.")
	} else if (umx_is_RAM(obj) ) {
		# This is a RAM Model: Set sane starting values
		# Means at manifest means
		# S at variance on diag, quite a bit less than cov off diag
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
				dataMeans = umx_means(theData[, manifests], ordVar = 0, na.rm = TRUE)
				freeManifestMeans = (obj@matrices$M@free[1, manifests] == TRUE)
				obj@matrices$M@values[1, manifests][freeManifestMeans] = dataMeans[freeManifestMeans]
				# covData = cov(theData, )
				covData = umx_cov_diag(theData[, manifests], ordVar = 1, format = "diag", use = "pairwise.complete.obs")
				covData = diag(covData)
			}
		} else {
			covData = diag(diag(theData))
		}
		# dataVariances = diag(covData)
		# ======================================================
		# = Fill the symmetrical matrix with good start values =
		# ======================================================
		# The diagonal is variances
		if(onlyTouchZeros) {
			freePaths = (obj@matrices$S@free[1:nVar, 1:nVar] == TRUE) & obj@matrices$S@values[1:nVar, 1:nVar] == 0
		} else {
			freePaths = (obj@matrices$S@free[1:nVar, 1:nVar] == TRUE)			
		}
		obj@matrices$S@values[1:nVar, 1:nVar][freePaths] = covData[freePaths]
		# ================
		# = set off diag =
		# ================
		# TODO decide whether to leave this as independence, or see with non-zero covariances...
		# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		# obj@matrices$S@values[1:nVar, 1:nVar][freePaths] = (covData[freePaths]/2)
		# offDiag = !diag(nVar)
		# newOffDiags = obj@matrices$S@values[1:nVar, 1:nVar][offDiag & freePaths]/3
		# obj@matrices$S@values[1:nVar, 1:nVar][offDiag & freePaths] = newOffDiags

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
	} else {
		stop("'obj' must be an mxMatrix, a RAM model, or a simple number")
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
#' @family Model Building Functions
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
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' umxGetParameters(m1) # Default "matrix address" labels, i.e "One Factor.S[2,2]"
#' m1 = umxLabel(m1)
#' umxGetParameters(m1, free = TRUE) # Informative labels: "G_to_x1", "x4_with_x4", etc.
#' # Labeling a matrix
#' a = umxLabel(mxMatrix(name = "a", "Full", 3, 3, values = 1:9))
#' a$labels
#' # labels with "data." in the name are left alone
#' a = mxMatrix(name = "a", "Full", 1,3, labels = c("data.a", "test", NA))
#' umxLabel(a, verbose = TRUE)
#' umxLabel(a, verbose = TRUE, overRideExisting = FALSE)
#' umxLabel(a, verbose = TRUE, overRideExisting = TRUE)
#' umxLabel(a, verbose = TRUE, overRideExisting = TRUE)
umxLabel <- function(obj, suffix = "", baseName = NA, setfree = FALSE, drop = 0, labelFixedCells = TRUE, jiggle = NA, boundDiag = NA, verbose = FALSE, overRideExisting = FALSE) {	
	# TODO check that arguments not used by a particular class are not set away from their defaults
	if (is(obj, "MxMatrix") ) { 
		# Label an mxMatrix
		xmuLabel_Matrix(mx_matrix = obj, baseName = baseName, setfree = setfree, drop = drop, labelFixedCells = labelFixedCells, jiggle = jiggle, boundDiag = boundDiag, suffix = suffix, verbose = verbose, overRideExisting = overRideExisting)
	} else if (umx_is_RAM(obj)) { 
		# Label a RAM model
		if(verbose){message("RAM")}
		return(xmuLabel_RAM_Model(model = obj, suffix = suffix, labelFixedCells = labelFixedCells, overRideExisting = overRideExisting, verbose = verbose))
	} else if (umx_is_MxModel(obj) ) {
		# Label a non-RAM matrix lamodel
		return(xmuLabel_MATRIX_Model(model = obj, suffix = suffix, verbose = verbose))
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
#' @family Model Building Functions
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
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1) # just run: will create saturated model if needed
#' m1 = umxRun(m1, setValues = TRUE, setLabels = TRUE) # set start values and label all parameters
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
	if(umx_is_RAM(model)){
		if(model@data@type == "raw"){
			# If we have a RAM model with raw data, compute the satuated and indpendence models
			# TODO: Update to omxSaturated() and omxIndependenceModel()
			message("computing saturated and independence models so you have access to absolute fit indices for this raw-data model")
			model_sat = umxSaturated(model, evaluate = TRUE, verbose = FALSE)
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
#' 
#' \code{fit2 = umxReRun(fit1, update = "Cs", name = "newModelName", comparison = TRUE)}
#' 
#' A powerful feature is regular expression. These let you drop collections of paths by matching patterns
#' fit2 = umxReRun(fit1, update = "C[sr]", regex = TRUE, name = "drop_Cs_andCr", comparison = TRUE)
#' 
#' If you're just starting out, you might find it easier to be more explicit. Like this: 
#' 
#' fit2 = omxSetParameters(fit1, labels = "Cs", values = 0, free = FALSE, name = "newModelName")
#' 
#' fit2 = mxRun(fit2)
#' 
#' @param lastFit  The \code{\link{mxModel}} you wish to update and run.
#' @param update What to update before re-running. Can be a list of labels, a regular expression (set regex = TRUE) or an object such as mxCI etc.
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
#' @family Model Building Functions
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
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = TRUE, setValues = TRUE)
#' m2 = umxReRun(m1, update = "G_to_x1", name = "drop_X1")
#' umxSummary(m2); umxCompare(m1, m2)
#' # 1-line version including comparison
#' m2 = umxReRun(m1, update = "G_to_x1", name = "drop_X1", comparison = TRUE)
#' m2 = umxReRun(m1, update = "^G_to_x[3-5]", regex = TRUE, name = "no_G_to_x3_5", comp = TRUE)
#' m2 = umxReRun(m1, update = "G_to_x1", value = .2, name = "fix_G_x1", comp = TRUE)
#' m3 = umxReRun(m2, update = "G_to_x1", free = TRUE, name = "free_G_x1_again")
#' umxCompare(m3, m2)

umxReRun <- function(lastFit, update = NULL, regex = FALSE, free = FALSE, value = 0, freeToStart = NA, name = NULL, verbose = FALSE, intervals = FALSE, comparison = FALSE, dropList = "deprecated") {
	if (dropList != "deprecated" | typeof(regex) != "logical"){
		if(dropList != "deprecated"){
			stop("hi. Sorry for the change, but please replace ", omxQuotes("dropList"), " with ", omxQuotes("update"),". e.g.:\n",
				"umxReRun(m1, dropList = ", omxQuotes("E_to_heartRate"), ")\n",
				"becomes\n",
				"umxReRun(m1, update = ", omxQuotes("E_to_heartRate"), ")\n",
   			 "\nThis regular expression will do it for you:\n",
   			 "find    = regex *= *(\\\"[^\\\"]+\\\"),\n",
   			 "replace = update = $1, regex = TRUE,"
			)
		} else {
			stop("hi. Sorry for the change. To use regex replace ", omxQuotes("regex"), " with ", omxQuotes("update"),
			 "AND regex =", omxQuotes(T), "e.g.:\n",
			 "umxReRun(m1, regex = ", omxQuotes("^E_.*"), ")\n",
			 "becomes\n",
			 "umxReRun(m1, update = ", omxQuotes("^E_.*"), ", regex = TRUE)\n",
			 "\nThis regular expression will do it for you:\n",
			 "find    = regex *= *(\\\"[^\\\"]+\\\"),\n",
			 "replace = update = $1, regex = TRUE,"
			 )
		}
	}

	if(is.null(update)){
		message("As you havn't asked to do anything: the parameters that are free to be dropped are:")
		print(umxGetParameters(lastFit))
		stop()
	}else{
		if(regex | typeof(update) == "character") {
			if (regex) {
				theLabels = umxGetParameters(lastFit, regex = update, free = freeToStart, verbose = verbose)
			}else {
				theLabels = update
			}
			x = omxSetParameters(lastFit, labels = theLabels, free = free, values = value, name = name)		
		} else {
			# TODO Label and start new object
			if(is.null(name)){ name = NA }
			x = mxModel(lastFit, update, name = name)
		}
		x = mxRun(x, intervals = intervals)
		if(comparison){
			if(free){ # we added a df
				umxCompare(x, lastFit)
			} else {
				umxCompare(lastFit, x)
			}
		}
		return(x)
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
#' @family Model Updating and Comparison
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
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1)
#' umxGetParameters(m1)
#' m1 = umxRun(m1, setLabels = TRUE)
#' umxGetParameters(m1)
#' umxGetParameters(m1, free = TRUE) # only the free parameter
#' umxGetParameters(m1, free = FALSE) # only parameters which are fixed
#' \dontrun{
#' # Complex regex patterns
#' umxGetParameters(m2, regex = "S_r_[0-9]c_6", free = TRUE) # Column 6 of matrix "as"
#' }
umxGetParameters <- function(inputTarget, regex = NA, free = NA, verbose = FALSE) {
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
		if(length(grep("[\\.\\*\\[\\(\\+\\|^]+", regex) ) < 1){ # no grep found: add some anchors for safety
			regex = paste0("^", regex, "[0-9]*$"); # anchor to the start of the string
			anchored = TRUE
			if(verbose == TRUE) {
				message("note: anchored regex to beginning of string and allowed only numeric follow\n");
			}
		}else{
			anchored=F
		}
		theLabels = grep(regex, theLabels, perl = FALSE, value = TRUE) # return more detail
		if(length(theLabels) == 0){
			msg = "Found no matching labels!\n"
			if(anchored == TRUE){
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
#' @family Model Updating and Comparison
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
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = TRUE, setValues = TRUE)
#' m2 = umxEquate(m1, master = "G_to_x1", slave = "G_to_x2", name = "Equate x1 and x2 loadings")
#' m2 = mxRun(m2) # have to run the model again...
#' umxCompare(m1, m2) # not good :-)
#' umxSummary(m1, m2) # not good :-)
umxEquate <- function(model, master, slave, free = c(TRUE, FALSE, NA), verbose = TRUE, name = NULL) {	
	free = umx_default_option(free, c(TRUE, FALSE, NA))
	if(!umx_is_MxModel(model)){
		message("ERROR in umxEquate: model must be a model, you gave me a ", class(model)[1])
		message("A usage example is umxEquate(model, master=\"a_to_b\", slave=\"a_to_c\", name=\"model2\") # equate paths a->b and a->c, in a new model called \"model2\"")
		stop()
	}
	if(length(grep("[\\^\\.\\*\\[\\(\\+\\|]+", master) ) < 1){ # no grep found: add some anchors
		master = paste0("^", master, "$"); # anchor to the start of the string
		slave  = paste0("^", slave,  "$");
		if(verbose == TRUE){
			cat("note: matching whole label\n");
		}
	}
	masterLabels = umxGetParameters(model, regex = master, free = free, verbose = verbose)
	slaveLabels  = umxGetParameters(model, regex = slave , free = free, verbose = verbose)
	if( length(slaveLabels) != length(masterLabels) && (length(masterLabels)!=1)) {
		print(list(masterLabels = masterLabels, slaveLabels = slaveLabels))
		stop("ERROR in umxEquate: master and slave labels not the same length!\n",
		length(slaveLabels), " slavelabels found, and ", length(masterLabels), " masters")
	}
	if(length(slaveLabels) == 0) {
		legal = names(omxGetParameters(model, indep=FALSE, free=free))
		legal = legal[which(!is.na(legal))]
		message("Labels available in model are: ", paste(legal, ", "))
		stop("ERROR in umxEquate: no slave labels found or none requested!")
	}
	print(list(masterLabels = masterLabels, slaveLabels = slaveLabels))
	model = omxSetParameters(model = model, labels = slaveLabels, newlabels = masterLabels, name = name)
	model = omxAssignFirstParameters(model, indep = FALSE)
	return(model)
}

#' umxFixAll
#'
#' Fix all free parameters in a model using omxGetParameters()
#'
#' @param model an \code{\link{mxModel}} within which to fix free parameters
#' @param verbose whether to mention how many paths were fixed (default is FALSE)
#' @param name optional new name for the model. if you begin with a _ it will be made a suffix
#' @param run  whether to fix and re-run the model, or just return it (defaults to FALSE)
#' @return - the fixed \code{\link{mxModel}}
#' @export
#' @family Model Updating and Comparison
#' @references - \url{https://github.com/tbates/umx}, \url{http://tbates.github.io}
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- umxRAM("OneFactor", data = mxData(cov(demoOneFactor), type = "cov", numObs = 500),
#' 	umxPath(latents, to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = latents, fixedAt = 1)
#' )
#' m1 = mxRun(m1)
#' m2 = umxFixAll(m1, run = TRUE, verbose = TRUE)
#' mxCompare(m1, m2)
umxFixAll <- function(model, name = "_fixed", run = FALSE, verbose= FALSE){
	if(!umx_is_MxModel(model)){
		message("ERROR in umxFixAll: model must be a model, you gave me a ", class(model)[1])
		message("A usage example is umxFixAll(model)")
		stop()
	}
	if(is.null(name)){
		name = model$name
	} else if("_" == substring(name, 1, 1)){
		name = paste0(model$name, name)
	}
	oldFree = names(omxGetParameters(model, indep = TRUE, free = TRUE))
	if(verbose){
		message("fixed ", length(oldFree), " paths")
	}
	model = omxSetParameters(model, oldFree, free = FALSE, strict = TRUE, name = name)
	if(run){
		model = mxRun(model)
	}
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
#' @family Model Updating and Comparison
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' \dontrun{
#' umxDrop1(fit3) # try dropping each free parameters (default)  
#' # drop "a_r1c1" and "a_r1c2" and see which matters more.
#' umxDrop1(model, regex="a_r1c1|a_r1c2")
#' }
umxDrop1 <- function(model, regex = NULL, maxP = 1) {
	if(is.null(regex)) {
		toDrop = umxGetParameters(model, free = TRUE)
	} else if (length(regex) > 1) {
		toDrop = regex
	} else {
		toDrop = grep(regex, umxGetParameters(model, free = TRUE), value = TRUE, ignore.case = TRUE)
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
#' @family Model Updating and Comparison
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
				# toAdd = umxGetParameters(model, free = FALSE)
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
	toAdd2 = toAdd[toAdd %in% umxGetParameters(model, free = FALSE)]
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
		tmp = omxSetParameters(model, labels = toAdd[i], free = TRUE, values = .01, name = paste0("add_", toAdd[i]))
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
		message(sum(good_rows, na.rm = TRUE), "of ", length(out$p), " items were beneath your p-threshold of ", maxP)
		message(sum(is.na(good_rows)), " was/were NA")
		good_rows[is.na(good_rows)] = T
		return(out[good_rows, ])
	}
}

# ===============
# = RAM Helpers =
# ===============

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
#' @family Model Building Functions
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
#' m1 = umxRun(m1, setValues = TRUE, setLabels = TRUE); umxSummary(m1, show="std")
#' umxPlot(m1)
#' 
#' m2 = mxModel("formative", type = "RAM",
#'	manifestVars = manifests,
#'	latentVars   = latents,
#'	# Factor loadings
#'	umxLatent("G", formedBy = manifests, data = theData),
#'	mxData(theData, type = "cov", numObs = nrow(demoOneFactor))
#' )
#' m2 = umxRun(m2, setValues = TRUE, setLabels = TRUE); umxSummary(m2, show="std")
#' umxPlot(m2)
#' }
umxLatent <- function(latent = NULL, formedBy = NULL, forms = NULL, data = NULL, type = NULL,  model.name = NULL, labelSuffix = "", verbose = TRUE, endogenous = "deprecated") {
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
	isCov = umx_is_cov(data, boolean = TRUE)
	if( any(!is.null(forms))) {
		manifests <- forms
	}else{
		manifests <- formedBy
	}
	if(isCov) {
		variances = diag(data[manifests, manifests])
	} else {
		manifestOrdVars = umx_is_ordered(data[,manifests])
		if(any(manifestOrdVars)) {
			means         = rep(0, times = length(manifests))
			variances     = rep(1, times = length(manifests))
			contMeans     = colMeans(data[,manifests[!manifestOrdVars], drop = F], na.rm = TRUE)
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
			means     = colMeans(data[, manifests], na.rm = TRUE)
			variances = diag(cov(data[, manifests], use = "complete"))
		}
	}

	if( any(!is.null(forms)) ) {
		# Handle forms case
		# p1 = Residual variance on manifests
		# p2 = Fix latent variance @ 1
		# p3 = Add paths from latent to manifests
		p1 = mxPath(from = manifests, arrows = 2, free = TRUE, values = variances)
		if(is.null(type)){ stop("Error in mxLatent: You must set type to either exogenous or endogenous when creating a latent variable with an outgoing path") }
		if(type == "endogenous"){
			# Free latent variance so it can do more than just redirect what comes in
			if(verbose){
				message(paste("latent '", latent, "' is free (treated as a source of variance)", sep=""))
			}
			p2 = mxPath(from=latent, connect="single", arrows=2, free= TRUE, values=.5)
		} else {
			# fix variance at 1 - no inputs
			if(verbose){
				message(paste("latent '", latent, "' has variance fixed @ 1"))
			}
			p2 = mxPath(from = latent, connect = "single", arrows = 2, free = Free, values = 1)
		}
		p3 = mxPath(from = latent, to = manifests, connect = "single", free = TRUE, values = variances)
		if(isCov) {
			# Nothing to do: covariance data don't need means...
			paths = list(p1, p2, p3)
		}else{
			# Add means: fix latent mean @0, and add freely estimated means to manifests
			p4 = mxPath(from = "one", to = latent   , arrows = 1, free = FALSE, values = 0)
			p5 = mxPath(from = "one", to = manifests, arrows = 1, free = TRUE, values = means)
			paths = list(p1, p2, p3, p4, p5)
		}
	} else {
		# Handle formedBy case
		# Add paths from manifests to the latent
		p1 = mxPath(from = manifests, to = latent, connect = "single", free = TRUE, values = umxValues(.6, n=manifests)) 
		# In general, manifest variance should be left free...
		# TODO If the data were correlations... we can inspect for that, and fix the variance to 1
		p2 = mxPath(from = manifests, connect = "single", arrows = 2, free = TRUE, values = variances)
		# Allow manifests to intercorrelate
		p3 = mxPath(from = manifests, connect = "unique.bivariate", arrows = 2, free = TRUE, values = umxValues(.3, n = manifests))
		if(isCov) {
			paths = list(p1, p2, p3)
		}else{
			# Fix latent mean at 0, and freely estimate manifest means
			p4 = mxPath(from="one", to=latent   , free = FALSE, values = 0)
			p5 = mxPath(from="one", to=manifests, free = TRUE, values = means)
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
				m1 <- mxModel(m1, umxThresholdRAMObjective(data, deviationBased = TRUE, droplevels = TRUE, verbose = TRUE))
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
	# m1 = mxLatent("Read", formedBy = manifestsRead, model.name="base"); umxPlot(m1, std = FALSE, dotFilename="name")
	# m2 = mxLatent("Read", forms = manifestsRead, as.model="base"); 
	# m2 <- mxModel(m2, mxData(cov(df), type="cov", numObs=100))
	# umxPlot(m2, std=FALSE, dotFilename="name")
	# mxLatent("Read", forms = manifestsRead)
}

umxConnect <- function(x) {
	# TODO handle endogenous
}

umxSingleIndicators <- function(manifests, data, labelSuffix = "", verbose = TRUE){
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
		p1 = mxPath(from = manifests, arrows = 2, values = variances)
		return(p1)
	} else {
		manifestOrdVars = umx_is_ordered(data[,manifests])
		if(any(manifestOrdVars)){
			means         = rep(0, times=length(manifests))
			variances     = rep(1, times=length(manifests))
			contMeans     = colMeans(data[,manifests[!manifestOrdVars], drop = F], na.rm= TRUE)
			contVariances = diag(cov(data[,manifests[!manifestOrdVars], drop = F], use="complete"))
			means[!manifestOrdVars] = contMeans				
			variances[!manifestOrdVars] = contVariances				
		}else{
			means     = colMeans(data[,manifests], na.rm = TRUE)
			variances = diag(cov(data[,manifests], use = "complete"))
		}
		# Add variance to the single manfests
		p1 = mxPath(from = manifests, arrows = 2, values = variances) # labels = mxLabel(manifests, suffix = paste0("unique", labelSuffix))
		# Add means for the single manfests
		p2 = mxPath(from = "one", to = manifests, values = means) # labels = mxLabel("one", manifests, suffix = labelSuffix)
		return(list(p1, p2))
	}
}

# ===========================
# = matrix-oriented helpers =
# ===========================

#' umxThresholdMatrix
#'
#' High-level helper for ordinal modeling. Creates, labels, and sets smart-starts for this complex matrix. Big time saver!
#'
#' When modeling ordinal data (sex, low-med-hi, 
#' depressed/normal, not at all, rarely, often, always), a useful conceptual strategy to handle expectations
#' is to build a standard-normal model (i.e., a latent model with zero-means, and unit (1.0) variances),
#' and then to threshold this normal distribution to generate the observed data. Thus an observation of "depressed"
#' is modeled as a high score on the latent normally distributed trait, with thresholds set so that only scores above
#' this threshold (1-minus the number of categories).
#'
#' For \strong{deviation methods}, it returns a list of lowerOnes_for_thresh, deviations_for_thresh & thresholdsAlgebra (named threshMatName)
#'
#' For \strong{direct}, it returns a thresholdsMatrix (named threshMatName)
#'
#' @param df the data being modelled (to allow access to the factor levels and quantiles within these for each variable)
#' @param suffixes e.g. c("T1", "T2") - Use for data with repeated observations in a row (i.e., twin data) (defaults to NA)
#' @param threshMatName name of the matrix which is returned. Defaults to "threshMat" - best not to change it.
#' @param method  How to set the thresholds: auto (the default), Mehta, which fixes the first two (auto chooses this for ordinal) or "allFree" (auto chooses this for binary)
#' @param l_u_bound c(NA, NA) by default, you can use this to bound the thresholds. Careful you don't set bounds too close if you do.
#' @param deviationBased Whether to build a helper matrix to keep the thresholds in order (defaults to = TRUE)
#' @param droplevels Whether to drop levels with no observed data (defaults to FALSE)
#' @param verbose (defaults to FALSE))
#' @return - thresholds matrix
#' @export
#' @family Miscellaneous Functions
#' @seealso - \code{\link{umxOrdinalObjective}}
#' @references - \url{https://github.com/tbates/umx}, \url{http://tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' x = data.frame(ordered(rbinom(100,1,.5))); names(x)<-c("x")
#' umxThresholdMatrix(x)
#' 
#' require(OpenMx)
#' data(twinData)
#' labList = c("MZFF", "MZMM", "DZFF", "DZMM", "DZOS")
#' twinData$zyg = factor(twinData$zyg, levels = 1:5, labels = labList)
#' # ==================
#' # = Binary example =
#' # ==================
#' # Cut to form category of 80 % obese subjects
#' cutPoints <- quantile(twinData[, "bmi1"], probs = .2, na.rm = TRUE)
#' obesityLevels = c('normal', 'obese')
#' twinData$obese1 <- cut(twinData$bmi1, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' twinData$obese2 <- cut(twinData$bmi2, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' # Step 2: Make the ordinal variables into mxFactors
#' # this ensures ordered= TRUE + requires user to confirm levels
#' selDVs = c("obese1", "obese2")
#' twinData[, selDVs] <- mxFactor(twinData[, selDVs], levels = obesityLevels)
#' mzData <- subset(twinData, zyg == "MZFF", selDVs)
#' str(mzData)
#' umxThresholdMatrix(mzData, suffixes = 1:2)
#' umxThresholdMatrix(mzData, suffixes = 1:2, verbose = FALSE) # supress informative messages
#' 
#' # ======================================
#' # = Ordinal (n categories > 2) example =
#' # ======================================
#' # Cut to form three categories of weight
#' cutPoints <- quantile(twinData[, "bmi1"], probs = c(.4, .7), na.rm = TRUE)
#' obesityLevels = c('normal', 'overweight', 'obese')
#' twinData$obeseTri1 <- cut(twinData$bmi1, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' twinData$obeseTri2 <- cut(twinData$bmi2, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' selDVs = c("obeseTri1", "obeseTri2")
#' twinData[, selDVs] <- mxFactor(twinData[, selDVs], levels = obesityLevels)
#' mzData <- subset(twinData, zyg == "MZFF", selDVs)
#' str(mzData)
#' umxThresholdMatrix(mzData, suffixes = 1:2)
#' umxThresholdMatrix(mzData, suffixes = 1:2, verbose = FALSE)
#'
#' # ========================================================
#' # = Mix of all three kinds example (and a 4-level trait) =
#' # ========================================================
#' 
#' cutPoints <- quantile(twinData[, "bmi1"], probs = c(.25, .4, .7), na.rm = TRUE)
#' obesityLevels = c('underWeight', 'normal', 'overweight', 'obese')
#' twinData$obeseQuad1 <- cut(twinData$bmi1, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' twinData$obeseQuad2 <- cut(twinData$bmi2, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' selDVs = c("obeseQuad1", "obeseQuad2")
#' twinData[, selDVs] <- mxFactor(twinData[, selDVs], levels = obesityLevels)
#' 
#' selDVs = umx_paste_names(c("bmi", "obese", "obeseTri", "obeseQuad"), "", 1:2)
#' mzData <- subset(twinData, zyg == "MZFF", selDVs)
#' str(mzData)
#' umxThresholdMatrix(mzData, suffixes = 1:2, verbose = FALSE)

umxThresholdMatrix <- function(df, suffixes = NA, threshMatName = "threshMat", method = c("auto", "Mehta", "allFree"), l_u_bound = c(NA, NA), deviationBased = TRUE, droplevels = FALSE, verbose = FALSE){
	if(droplevels){ stop("Not sure it's wise to drop levels...") }
	# TODO implement manual choice of method - more flexible and explicit.
	method      = umx_default_option(method, c("auto", "Mehta", "allFree"), check = TRUE)
	nSib        = length(suffixes)
	isFactor    = umx_is_ordered(df) # all ordered factors including binary
	isOrd       = umx_is_ordered(df, ordinal.only = TRUE) # only ordinals
	isBin       = umx_is_ordered(df, binary.only  = TRUE) # only binaries
	nFactors    = sum(isFactor)
	nOrdVars    = sum(isOrd)
	nBinVars    = sum(isBin)
	factorVarNames = names(df)[isFactor]
	ordVarNames    = names(df)[isOrd]
	binVarNames    = names(df)[isBin]
	if((nOrdVars + nBinVars) < 1){
		message("No ordinal or binary variables in dataframe: no need to call umxThresholdMatrix")
		return(NA) # probably OK to set thresholds matrix to NA in mxExpectation()
		# TODO check if we should die here instead
	} else {
		if(verbose){
			message("'threshMat' created to handle ")
			if(nSib == 2){
				if(nOrdVars > 0){
					message(nOrdVars/nSib, " pair(s) of ordinal variables:", omxQuotes(ordVarNames), "\n")
				}
				if(nBinVars > 0){
					message(nBinVars/nSib, " pair(s) of binary variables:", omxQuotes(binVarNames), "\n")
				}
			} else {
				if(nOrdVars > 0){
					message(nOrdVars, " ordinal variables:", omxQuotes(ordVarNames), "\n")
				}
				if(nBinVars > 0){
					message(nBinVars, " binary variables:", omxQuotes(binVarNames), "\n")
				}
			}
		}
	}
	minLevels = xmuMinLevels(df)
	maxLevels = xmuMaxLevels(df)
	maxThresh = maxLevels - 1

	# TODO simplify for n = bin, n= ord, n= cont msg
	if(sum(isBin) > 0){
		binVarNames = names(df)[isBin]
		if(verbose){
			message(sum(isBin), " trait(s) are binary (only 2-levels).\n",
			omxQuotes(binVarNames),
			"\nFor these, you you MUST fix or constrain (usually @mean=0 & var=1) the latent traits driving each ordinal variable.\n",
			"See ?mxThresholdMatrix")
		}
	} else if(minLevels > 2){
		if(verbose){
			message("All factors have at least three levels. I will use Paras Mehta's 'fix first 2 thresholds' method.\n",
			"It's ESSENTIAL that you leave FREE the means and variances of the latent ordinal traits!!!\n",
			"See ?mxThresholdMatrix")
		}
	} else {
		stop("You seem to have a trait with only one category... makes it a bit futile to model it?")
	}

	df = df[, factorVarNames, drop = FALSE]

	if(nSib == 2){
		# For better precision, copy both halves of the dataframe into each
		T1 = df[, grep(paste0(suffixes[1], "$"), factorVarNames, value = TRUE), drop = FALSE]
		T2 = df[, grep(paste0(suffixes[2], "$"), factorVarNames, value = TRUE), drop = FALSE]
		names(T2) <- names(T1)
		dfLong = rbind(T1, T2)
		df = cbind(dfLong, dfLong)
		names(df) = factorVarNames
	} else if(nSib == 1){
		# df is fine as is.		
	} else {
		stop("I can only handle 1 and 2 sib models. You gave me ", nSib, " suffixes.")
	}
	
	# size the matrix maxThresh rows * nFactors cols
	threshMat = mxMatrix(name = threshMatName, type = "Full",
		nrow     = maxThresh,
		ncol     = nFactors,
		free     = TRUE, values = rep(NA, (maxThresh * nFactors)),
		lbound   = l_u_bound[1],
		ubound   = l_u_bound[2],
		dimnames = list(paste0("th_", 1:maxThresh), factorVarNames)
	)

	# For each factor variable
	for (thisVarName in factorVarNames) {
		thisCol = df[,thisVarName]
		nThreshThisVar = length(levels(thisCol)) -1 # "0"  "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10" "11" "12"

		# nls is too fragile...
		# tmp = data.frame(zValues = zValues)
		# tmp$x = c(1:length(zValues))
		# fit <- nls(zValues ~ theta1/(1 + exp(-(theta2 + theta3*x))), start=list(theta1 = 4, theta2 = 0.09, theta3 = 0.31), data =tmp)
		# zValues = predict(fit)
		# http://stats.stackexchange.com/questions/74544/fitting-a-sigmoid-function-why-is-my-fit-so-bad
		# tryCatch({
		# 	# job here is just to interpolate empty cell frequencies..
		# }, warning = function(cond) {
		#     # warning-handler-code
		#     umx_msg(thisVarName)
		# 	        umx_msg(zValues)
		# 	message(cond)
		# }, error = function(cond) {
		#     umx_msg(thisVarName)
		# 	        umx_msg(zValues)
		# 	        message(cond)
		# }, finally = {
		#     # cleanup-code
		# })

		# This approach would work differently from the start...
		# co = coef(fitdistr(thisCol[!is.na(thisCol)], "normal"))
		# dnorm(1:(nThreshThisVar+1), co["mean"], co["sd"]))
		
		# ===============================================================
		# = Work out z-values for thresholds based on simple bin counts =
		# ===============================================================
		# Pros: Doesn't assume equal intervals.
		# Problems = empty bins and noise (equal thresholds (illegal) and higher than realisitic z-values)
		tab = table(thisCol)/sum(table(thisCol)) # Simple histogram of proportion at each threshold
		cumTab = cumsum(tab)                     # Convert to a cumulative sum (sigmoid from 0 to 1)
		# Use quantiles to get z-equivalent for each level: ditch one to get thresholds...
		zValues = qnorm(p = cumTab, lower.tail = TRUE)
		# take this table as make a simple vector
		zValues = as.numeric(zValues)
		# =======================================================================================
		# = TODO handle where flows over, say, 3.3... squash the top down or let the user know? =
		# =======================================================================================
		if(any(is.infinite(zValues))){
			nPlusInf  = sum(zValues == (Inf))
			nMinusInf = sum(zValues == (-Inf))
			if(nPlusInf){
				maxOK = max(zValues[!is.infinite(zValues)])
				padding = seq(from = (maxOK + .1), by = .1, length.out = nPlusInf)
				zValues[zValues == (Inf)] = padding
			}
			if(nMinusInf){
				minOK = min(zValues[!is.infinite(zValues)])
				padding = seq(from = (minOK - .1), by = (- .1), length.out = nMinusInf)
				zValues[zValues == (-Inf)] = padding
			}
		}
		# =================================
		# = Move duplicates (empty cells) =
		# =================================
		if(any(duplicated(zValues))){
			# message("You have some empty cells")
			# Find where the values change:
			runs         = rle(zValues)
			runLengths   = runs$lengths
			runValues    = runs$values
			distinctCount = length(runValues)
			indexIntoRLE = indexIntoZ = 1
			for (i in runLengths) {
				runLen = i
				if(runLen != 1){
					repeatedValue   = runValues[indexIntoRLE]
					preceedingValue = runValues[(indexIntoRLE - 1)]
					minimumStep = .01
					if(indexIntoRLE == distinctCount){
						newValues = seq(from = (preceedingValue + minimumStep), by = (minimumStep), length.out = runLen)
						zValues[c(indexIntoZ:(indexIntoZ + runLen - 1))] = rev(newValues)
					} else {
						followedBy = runValues[(indexIntoRLE + 1)]
						minimumStep = min((followedBy - preceedingValue)/(runLen + 1), minimumStep)
						newValues = seq(from = (followedBy - minimumStep), by = (-minimumStep), length.out = runLen)
						zValues[c(indexIntoZ:(indexIntoZ + runLen - 1))] = rev(newValues)
					}
				}
				indexIntoZ   = indexIntoZ + runLen
				indexIntoRLE = indexIntoRLE + 1
				# Play "The chemistry between them", Dorothy Hodgkin
				# Copenhagen, Michael Frein
			}
		}
        # TODO start from 1, right, not 2?
		values = c(zValues[1:(nThreshThisVar)], rep(NA, (maxThresh - nThreshThisVar)))
		sortValues <- sort(values, na.last = TRUE)
		if (!identical(sortValues, values)) {
			umx_msg(values)
			stop("The thresholds for ", thisVarName, " are not in order... oops: that's my fault :-(")
		}
		# ==============
		# = Set labels =
		# ==============
		if(nSib == 2){
			# search string to find all sib's versions of a var
			findStr = paste0( "(", paste(suffixes, collapse = "|"), ")$")
			thisLab = sub(findStr, "", thisVarName)		
		} else {
			thisLab = thisVarName
		}	
        labels = c(paste0(thisLab, "_thresh", 1:nThreshThisVar), rep(NA, (maxThresh - nThreshThisVar)))
        
		# ============
		# = Set Free =
		# ============
		free = c(rep(TRUE, nThreshThisVar), rep(FALSE, (maxThresh - nThreshThisVar)))
		
		if(nThreshThisVar > 1){ # fix the first 2
			free[1:2] = FALSE
		}
		
		threshMat$labels[, thisVarName] = labels
		threshMat$free[,   thisVarName] = free
		threshMat$values[, thisVarName] = values
	} # end for each factor variable
	
	
	if(deviationBased) {
		# ==========================
		# = Adding deviation model =
		# ==========================
		# threshMat = mxMatrix(name = threshMatName, type = "Full",
		# 	nrow     = maxThresh,
		# 	ncol     = nFactors,
		# 	free     = TRUE, values = rep(NA, (maxThresh * nFactors)),
		# 	lbound   = l_u_bound[1],
		# 	ubound   = l_u_bound[2],
		# 	dimnames = list(paste0("th_", 1:maxThresh), factorVarNames)
		# )
		
		# TODO:
		# 1. Convert thresholds into deviations
		# value 1 for each var = the base, everything else is a deviation
		# 2. Make matrix deviations_for_thresh (similar to existing threshMat), fill values with results from 1
		# 3. Make a lower matrix of 1s called "lowerOnes_for_thresh"
		# 4. Create thresholdsAlgebra named threshMatName
		# 5. Return a package of lowerOnes_for_thresh, deviations_for_thresh & thresholdsAlgebra (named threshMatName)
# 1
		# startDeviations
		startDeviations = threshMat$values
		nrows = dim(threshMat$values)[1]
		ncols = dim(threshMat$values)[2]
		if (nrows > 1){
			for (col in 1:ncols) {
				# Skip row 1 which is the base
				for (row in 2:nrows) {
					# Convert remaining rows to offsets
					startDeviations[row, col] = (threshMat$values[row, col] - threshMat$values[(row-1), col])
				}
			}
		}
		# threshMat$values
		#          obese1 obeseTri1 obeseQuad1     obese2 obeseTri2 obeseQuad2
		# th_1 -0.4727891 0.2557009 -0.2345662 -0.4727891 0.2557009 -0.2345662
		# th_2         NA 1.0601180  0.2557009         NA 1.0601180  0.2557009
		# th_3         NA        NA  1.0601180         NA        NA  1.0601180
		#
		# threshMat$free
		#      obese1 obeseTri1 obeseQuad1 obese2 obeseTri2 obeseQuad2
		# th_1   TRUE     FALSE      FALSE   TRUE     FALSE      FALSE
		# th_2  FALSE     FALSE      FALSE  FALSE     FALSE      FALSE
		# th_3  FALSE     FALSE       TRUE  FALSE     FALSE       TRUE
	
# 2
		deviations_for_thresh = mxMatrix(name = "deviations_for_thresh", type = "Full",
			nrow     = maxThresh, ncol = nFactors,
			free     = threshMat$free, values = startDeviations,
			lbound   = .001,
			ubound   = NA,
			dimnames = list(paste0("dev_", 1:maxThresh), factorVarNames)
		)
# 2
		lowerOnes_for_thresh = mxMatrix(name = "lowerOnes_for_thresh", type = "Lower", nrow = maxThresh, free = FALSE, values = 1)
# 3
		threshDimNames = list(paste0("th_", 1:maxThresh), factorVarNames)
		thresholdsAlgebra = mxAlgebra(name = threshMatName, lowerOnes_for_thresh %*% deviations_for_thresh, dimnames = threshDimNames)

		return(list(lowerOnes_for_thresh, deviations_for_thresh, thresholdsAlgebra))
	} else {
		return(threshMat)
	}
}

#' umxOrdinalObjective
#'
#' High-level helper for ordinal modeling. Creates, labels, and sets smart-starts for this complex matrix. I think
#' I will deprecate this, as it does too little and hides too much to be worth supporting...
#'
#' When modeling ordinal data (sex, low-med-hi, depressed/normal, not at all, rarely, often, always),
#' a useful conceptual strategy to handle expectations
#' is to build a standard-normal model (i.e., a latent model with zero-means, and unit (1.0) variances),
#' and then to threshold this normal distribution to generate the observed data. Thus an observation of "depressed"
#' is modeled as a high score on the latent normally distributed trait, with thresholds set so that only scores above
#' this threshold (1-minus the number of categories).
#'
#' @param df the data being modelled (to allow access to the factor levels and quantiles within these for each variable)
#' @param suffixes e.g. c("T1", "T2") - Use for data with repeated observations in a row (i.e., twin data) (defaults to NA)
#' @param covName is the name of the expected-covariance matrix (Defaults to "expCov")
#' @param meansName is the name of the expected-means matrix (Defaults to "expMeans") 
#' @param threshMatName = "threshMat"
#' @param vector = FALSE
#' @param deviationBased Whether to build a helper matrix to keep the thresholds in order (defaults to = FALSE)
#' @param droplevels Whether to drop levels with no observed data (defaults to FALSE)
#' @param verbose (defaults to FALSE))
#' @return - list of thresh matrix, fit function, and expectation.
#' @export
#' @family Model Building Functions
#' @family umx deprecated
#' @seealso - \code{\link{umxThresholdMatrix}}
#' @references - \url{https://github.com/tbates/umx}, \url{http://tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' \dontrun{
#' umxOrdinalObjective(df, suffixes = c("_T1", "_T2"))
#' }
umxOrdinalObjective <- function(df, suffixes = NA, covName = "expCov", meansName = "expMean", threshMatName = "threshMat", vector = FALSE, deviationBased = FALSE, droplevels = FALSE,  verbose = FALSE){
	threshMat = umxThresholdMatrix(df, suffixes = suffixes, threshMatName = threshMatName, deviationBased = deviationBased, droplevels = droplevels, verbose = verbose)
	expect    = mxExpectationNormal(covariance = covName, means = meansName, dimnames = nameList, thresholds = threshMatName)
	fit       = mxFitFunctionML(vector = vector)
	list(threshMat, expect, fit)
}

# ===========
# = Utility =
# ===========


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
	CIlist   = umxGetParameters(model ,regex= "[ace][0-9]", verbose= FALSE)
	thisCI   = CIlist[CInumber]
	model    = mxModel(model, mxCI(thisCI) )
	return (model)
}

#' umxPath: Flexible specification of sem paths
#'
#' @details This function returns a standard mxPath, but gives new options for specifying the path. In addition to the normal
#' from and to, it adds specialised parameters for variances (var), two headed paths (with) and means (mean).
#' There are also verbs for fixing values: "fixedAt" and "fixFirst"
#' Finally, it also allows sem-style "A->B" string specification.
#'
#' @description The goal of this function is to enable quck-to-write, quick-to-read, flexible path descriptions for RAM models in OpenMx.
#' 
#' It introduces 10 new verbs: \strong{with}, \strong{var}, \strong{cov}, \strong{unique.bivariate}, \strong{Cholesky}, \strong{means}, \strong{v1m0}, \strong{fixedAt}, \strong{freeAt}, \strong{firstAt}.
#' 
#' The new key "with" means you no-longer need set arrows = 2 on covariances. So you can say:
#'
#'    \code{umxPath(A, with = B)} instead of \code{mxPath(from = A, to = B, arrows = 2)}.
#' 
#' Specify a variance for A with
#' 
#' \code{umxPath(var = A)}.
#' 
#' This is equivalent to \code{mxPath(from = A, to = A, arrows = 2)}.
#' 
#' To specify a mean, you just say
#' 
#' \code{umxPath(mean = A)}, which is equivalent to \code{mxPath(from = "one", to = A)}.
#' 
#' To fix a path at a value, instead of to \code{mxPath(from = A, to = A, arrows = 2, free = FALSE, values = 1)} you can say:
#' 
#' \code{umxPath(var = A, fixedAt = 1)} .
#' 
#' The common task of creating a variable with variance fixed at 1 and mean at 0, you can simply say:
#' 
#' \code{umxPath(v1m0 = A)}
#' 
#' umxPath exposes unique.parameter as a parameter so you don't have remember how to fill in connect= in mxPath
#' All unique bivariate paths can be specified using unique.bivariate
#' 
#' \code{umxPath(c('A',"B","C"), unique.bivariate = TRUE)} to create paths A<->B, B<->C, and A<->C.
#' 
#' Finally, you can create \emph{Cholesky} form paths (see \code{\link{umxACE}})
#'
#' \code{umxPath(c("A1", "A2"), to c("var1", "var2"), Cholesky = TURE)}
#' 
#' Setting up a latent trait, you can fix the loading of the first path with
#' 
#' \code{mxPath(A, to = c(B,C,D), fixFirst = TRUE)}  
#' 
#' This is equivalent to \code{mxPath(from = A, to = c(B,C,D), free = c(F, T, T), values = c(1, .5, .4))}.
#' 
#' Finally, in the future I will implement the John Fox "sem" package style notation, i.e., "A -> B".
#' If you want to add multiple paths that way, separate them with a semi-colon or a return (see examples below.)
#' 
#' 
#' @param from either a source variable e.g "A" or c("A","B"), OR a sem-style path description, e.g. "A-> B" or "C <> B"
#' @param to one or more target variables for one-headed paths, e.g "A" or c("A","B") 
#' @param with same as "to = vars, arrows = 2". nb: from, to= and var=  must be left empty (their default)
#' @param var equivalent to setting "from = vars, arrows = 2". nb: from, to, and with must be left empty (their default)
#' @param cov equivalent to setting "from = X, to = Y, arrows = 2". nb: from, to, and with must be left empty (their default)
#' @param unique.bivariate equivalent to setting "connect = "unique.bivariate", arrows = 2". nb: from, to, and with must be left empty (their default)
#' @param formative Paired with to, this will build a formative variable, from the formatives, allowing these to
#' covary, and to the latent "to" variable, fixing its variance to zero.
#' @param Cholesky Treat the \strong{from} vars as latent and \strong{to} as measured, and connect up as in an ACE model.
#' @param means equivalent to "from = 'one', to = x. nb: from, to, with and var must be left empty (their default).
#' @param v1m0 variance of 1 and mean of zero in one call.
#' @param fixedAt Equivalent to setting "free = FALSE, values = x" nb: free and values must be left empty (their default)
#' @param freeAt Equivalent to setting "free = TRUE, values = x" nb: free and values must be left empty (their default)
#' @param firstAt first value is fixed at this (values passed to free are ignored: warning if not a single TRUE)
#' @param connect as in mxPath - nb: Only used when using from and to
#' @param arrows as in mxPath - nb: Only used when using from and to
#' @param free whether the value is free to be optimised
#' @param values default value list
#' @param labels labels for each path
#' @param lbound lower bounds for each path value
#' @param ubound upper bounds for each path value
#' @return - \code{\link{mxPath}}
#' @export
#' @family Model Building Functions
#' @seealso - \code{\link{umxLabel}}, \code{\link{mxMatrix}}, \code{\link{umxStart}}
#' @references - \url{https://github.com/tbates/umx}, \url{http://tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
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
#' # A worked example
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' myData = mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' m1 <- umxRAM("One Factor", data = myData,
#' 	umxPath(latents, to = manifests),
#' 	# umxPath("G -> manifests"),
#' 	umxPath(var = manifests),
#' 	umxPath(var = latents, fixedAt = 1.0)
#' )
#' m1 = mxRun(m1)
#' umxSummary(m1, show = "std")

# # These are not yet implemented!!
# #' umxPath("A <-> B") # same path as above using a string
# #' umxPath("A -> B") # one-headed arrow with string syntax
# #' umxPath("A <> B; A <-- B") # This is ok too
# #' umxPath("A -> B; B>C; C --> D") # two paths. white space and hyphens not needed
# #' # manifests is a reserved word, as is latents.
# #' # It allows the string syntax to use the manifestVars variable
# #' umxPath("A -> manifests") 

umxPath <- function(from = NULL, to = NULL, with = NULL, var = NULL, cov = NULL, unique.bivariate = NULL, formative = NULL, Cholesky = NULL, means = NULL, v1m0 = NULL, fixedAt = NULL, freeAt = NULL, firstAt = NULL, connect = "single", arrows = 1, free = TRUE, values = NA, labels = NA, lbound = NA, ubound = NA) {
	if(!is.null(formative)){
		stop("I haven't implemented formative yet... still thinking about whether its a good idea or a bad idea")
	}
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
		stop("At most one of with, cov, var, unique.bivariate, v1m0, and means can be set: Use at one time")
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
		if(!is.null(from)){
			stop("To use unique.bivariate, 'from=' should be empty.\n",
			"Just say 'unique.bivariate = c(\"X\",\"Y\").'\n",
			"or 'unique.bivariate = c(\"X\",\"Y\"), to = \"Z\"")
		} else {
			if(is.null(to)){
				to = NA				
			} else {
				to = to	
			}
			from    = unique.bivariate
			arrows  = 2
			connect = "unique.bivariate"
		}
	} else if(!is.null(Cholesky)){
		if(is.null(from) | is.null(to)){
			stop("To use Cholesky, I need both 'from=' and 'to=' to be set.\n")
		} else {
			stop("I have not yet implemented Cholesky as a connection - email me a reminder!.\n")
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
	if(sum(c(is.null(fixedAt), is.null(firstAt), is.null(freeAt))) < 2){
		stop("At most one of fixedAt freeAt and firstAt can be set: You seem to have tried to set more than one.")
	}

	# Handle firstAt
	if(!is.null(firstAt)){
		if(length(from) > 1 && length(to) > 1){
			# TODO think about this
			stop("It's not wise to use firstAt with multiple from sources AND multiple to targets. I'd like to think about this before implementing it..")
		} else {
			numPaths = max(length(from), length(to))
			free    = rep(TRUE, numPaths)
			free[1] = FALSE
			values    = rep(NA, numPaths)
			values[1] = firstAt
		}
	}	
	# Handle fixedAt
	if(!is.null(fixedAt)){
		free = FALSE
		values = fixedAt
	}

	# Handle freeAt
	if(!is.null(freeAt)){
		free = TRUE
		values = freeAt
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
#' @family Model Building Functions
#' @references - \url{https://github.com/tbates/umx}, \url{http://tbates.github.io}
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
# =====================================
# = Parallel helpers to be added here =
# =====================================

#' Helper functions for OpenMx
#'
#' umx allows you to more easily build, run, modify, and report models using OpenMx
#' with code. The core functions are linked below under \strong{See Also}
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
#' @family Model Building Functions
#' @family Reporting Functions
#' @family Model Updating and Comparison
#' @family Advanced Helpers
#' @family Miscellaneous Functions
#' @family Miscellaneous Utility Functions
#' @family Miscellaneous Stats Helpers
#' @family Twin Modeling Functions
#' @family Twin Reporting Functions
#' @family umx deprecated
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
#' 	mxPath(from = latents  , arrows = 2, free = FALSE, values = 1),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = nrow(demoOneFactor))
#' )
#' 
#' omxGetParameters(m1) # nb: By default, paths have no labels, and starts of 0
#' 
#' # With \code{link{umxLabel}}, you can easily add informative and predictable labels to each free
#' # path (works with matrix style as well!) and use \code{link{umxValues}}, to set 
#' # sensible guesses for start values...
#' m1 = umxLabel(m1)  
#' m1 = umxValues(m1)  
#' 
#' # Re-run omxGetParameters...
#' omxGetParameters(m1) # Wow! Now your model has informative labels, & better starts
#' 
#' # umxRun the model (calculates saturated models for raw data, & repeats
#' # if the model is not code green)
#' m1 = umxRun(m1, setLabels = TRUE, setValues = TRUE) # not needed given we've done this above.
#' # But you can see how umxRun enables 1-line setup and run
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
#' m2 = omxSetParameters(m1, labels = "G_to_x1", values = 0, free = FALSE, name = "no_g_on_X5")
#' m2 = mxRun(m2)
#' # Compare the two models
#' umxCompare(m1, m2)
#' 
#' # Use umxReRun to do the same thing in 1-line
#' m2 = umxReRun(m1, "G_to_x1", name = "no_effect_of_g_on_X5", comparison = TRUE)
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
#' # umxPlot(m1, std = TRUE)
#' # Run this instead if you don't have GraphViz
#' plot(m1, std = TRUE, dotFilename = NA)
#' @docType package
#' @name umx
NULL


#' mxTryHarder
#'
#' Rob's fn with mods from Charles
#'
#' @param model an \code{\link{mxModel}} to try and get to run
#' @param extraTries = 10
#' @param greenOK  Whether it's OK to have GREEN warngings (Defaults to FALSE)
#' @param loc = 1
#' @param scale = 0.25
#' @param checkHess = TRUE
#' @param fit2beat = Inf
#' @param paste how to output results? (Deafult = TRUE)
#' @param iterationSummary = FALSE
#' @param bestInitsOutput = FALSE
#' @param showInits = FALSE
#' @param ... things passed to mxRun?
#' @return - \code{\link{mxModel}}
#' @export
#' @family umx core functions
#' @references - TODO this is Rob's fn with mods from Charles
#' @examples
#' \dontrun{
#' model = mxTryHarder(model)
#' }
mxTryHarder <- function(model, extraTries = 10, greenOK = FALSE, loc = 1, scale = 0.25, checkHess = TRUE, fit2beat = Inf, paste = TRUE, iterationSummary=FALSE, bestInitsOutput=FALSE, showInits=FALSE, ...) {
  stopflag <- FALSE
  numdone  <- 0
  bestfitsofar <- Inf
  inits <- omxGetParameters(model)
  while (!stopflag) {
    message(paste0('\nBegin fit attempt ', numdone+1, ' of at maximum ', extraTries +1, ' tries'))
    params <- omxGetParameters(model)
    if(showInits == TRUE) {
      message('Starting values:  ')
      print(params)
    }
    numdone <- numdone + 1
    
    fit <- suppressWarnings(try(mxRun(model, suppressWarnings = T, unsafe=T, silent=T,intervals=FALSE)))
    if (class(fit) == "try-error" || fit$output$status$status== -1) {
      newparams<-omxGetParameters(model) #get recent fit
      if(exists('bestfit')) newparams<-bestfit.params #if bestfit exists use this instead
      if(numdone %% 3 == 0) newparams<-inits #sometimes, use initial start values instead
      if(numdone %% 5 == 0) { #sometimes, switch optimizers
        if(mxOption(NULL, "Default optimizer")=='CSOLNP') newoptimizer<-'NPSOL'
        if(mxOption(NULL, "Default optimizer")=='NPSOL') newoptimizer<-'CSOLNP'
        message(paste0("Switching to ',newoptimizer,' optimizer for OpenMx temporarily")) 
        mxOption(NULL, "Default optimizer", newoptimizer)
      }
      model <- omxSetParameters(model, labels = names(newparams), 
        values = newparams * rnorm(length(newparams), loc, scale))  # set to multiply bestfit.params instead of params, changed to rnorm, made loc and scale separate.
    }
    else { #if fit was not an error
      if (fit$output$minimum <= bestfitsofar) {
        bestfit <- fit
        bestfit.params <- omxGetParameters(bestfit)
      }
      
      if(fit$output$minimum < bestfitsofar) bestfitsofar <- fit$output$minimum
      
      if (length(fit$output$calculatedHessian) == 0) {
        checkHess <- FALSE
      }
      if (checkHess) {
        if (sum(is.na(fit$output$calculatedHessian)) > 
            0) {
          checkHess <- FALSE
        }
      }
      if(all(fit$output$calculatedHessian == 0)) checkHess<-FALSE #had problems with multigroup
      
      stopflag <- ifelse(checkHess, (fit$output$status[[1]] <= 
          greenOK) & (all(eigen(fit$output$calculatedHessian, 
            symmetric = T, only.values = T)$values > 0)) & 
          (fit$output$minimum <= fit2beat) & (fit$output$minimum <= bestfitsofar), (fit$output$status[[1]] <=  #added bestfitsofar condition
            greenOK) & (fit$output$minimum <= fit2beat) & (fit$output$minimum <= bestfitsofar) )
      if (!stopflag) {
        model <- fit
        newparams<-omxGetParameters(fit) #get recent fit
        if(exists('bestfit')) newparams<-bestfit.params #if bestfit exists use this instead
        if(numdone %% 2 ==0) newparams<-inits #if even iterations, use initial start values instead

        model <- omxSetParameters(model, labels = names(params), 
          values = newparams * rnorm(length(newparams),loc, scale))
        fit2beat <- ifelse(fit$output$minimum < fit2beat, fit$output$minimum, 
          fit2beat)
        if(iterationSummary==TRUE){
          message(paste0("Attempt ",numdone," fit:  "))
          message(paste(names(params),": ", fit$output$estimate,"\n"))
          message(paste0("-2LL = ", fit$output$Minus2LogLikelihood))
        }
      }
      
      if(stopflag){
        if (length(summary(fit)$npsolMessage) > 0) {
          warning(summary(fit)$npsolMessage)
        }
        fit<-bestfit
        if(length(fit$intervals)>0){
            message("Setting NPSOL optimizer for confidence interval estimation") 
            mxOption(NULL, "Default optimizer", "NPSOL")
			fit <- suppressWarnings(mxRun(fit,intervals=TRUE,suppressWarnings=T,silent=T))
        }
        params <- bestfit.params
        message('\nSolution found\n')
        if(iterationSummary == TRUE){
			message(paste(names(bestfit.params),": ", bestfit$output$estimate,"\n"))
		  message(paste0("-2LL = ", bestfit$output$Minus2LogLikelihood))
        }        
      }
    } #end 'if fit not an error' section
    if (numdone > extraTries) {
      message('\nRetry limit reached')
      stopflag <- TRUE
      if (exists("bestfit")) {
        fit <- bestfit
        params <- bestfit.params
        if (length(summary(fit)$npsolMessage) > 0) { 
          warning(summary(fit)$npsolMessage)
        }
        message('\nUncertain solution found - consider parameter validity, try again, increase retryAttempts, change inits, change model, or check data!\n')
        if(iterationSummary==TRUE){
          message(paste(names(bestfit.params),": ", bestfit$output$estimate,"\n"))
          message(paste0("-2LL = ", bestfit$output$Minus2LogLikelihood))
        }
      }
    }
  }
  if(bestInitsOutput){
	  message("Start values from best fit:")
	  if(paste){
		  message(paste(params, sep = ",", collapse = ","))
	  }else{
		  message(paste(names(params),": ", params,"\n"))
	  }
  }
  return(fit)
}
