#' Example code from Twin Research and Human Genetics Paper on umx
#'
#' @description
#' This is the example code used in our Twin Research and Human Genetics Paper on `umx`
#'
#' @export
#' @seealso - [umx()]
#' @references - Bates, T. C., Neale, M. C., & Maes, H. H. (2019). 
#' umx: A library for Structural Equation and Twin Modelling in R. 
#' *Twin Research and Human Genetics*, **22**, 27-41. DOI: [https://doi.org/10.1017/thg.2019.2](https://doi.org/10.1017/thg.2019.2)
#' @md
#' @aliases umxExampleCode_TRHGpaper
#' @examples
#' \dontrun{
#' 
#' # ==========================================================================
#' # = Example code from Twin Research and Human Genetics Paper on umx(model) =
#' # ==========================================================================
#' 
#' # Installing umx can be done using the R-code:
#' install.packages("umx")
#' # load as usual
#' library("umx")
#' 
#' # The current package version can be shown with:
#' umxVersion("umx")
#' 
#' # Get the latest NPSOL and multi-core build of OpenMx
#' install.OpenMx("NPSOL")
#' 
#' # Bleeding edge version of OpenMx for MacOS
#' install.OpenMx("travis")
#' 
#' # ============
#' # = CFA Code =
#' # ============
#' 
#' # Load the umx library (this is assumed in subsequent examples
#' library("umx")
#'
#' # Load demo data consisting of 5 correlated variables, x1:x5 
#' data(demoOneFactor)
#' 
#' # Create a list of the manifest variables for use in specifying the model
#' manifests = paste0("x", 1:5) # 'x1', 'x2', ...'x5'
#' 
#' # Create model cfa1, with name 'CFA', data demoOneFactor, and the CFA paths.
#' 
#' cfa1 = umxRAM("CFA", data = demoOneFactor,
#' 	# Create latent variable 'G', with fixed variance of 1 and mean of 0
#' 	umxPath(v1m0 = "G"),
#' 	# Create 5 manifest variables, x1:x5, with free variance and mean
#' 	umxPath(v.m. = manifests),
#' 	# Create 1-headed paths from G to each of the manifests
#' 	umxPath("G", to = manifests)
#' )
#' 
#' # ====================
#' # = Parameter labels =
#' # ====================
#' 
#' x = umxLabel(mxMatrix(name="means", "Full", ncol = 2, nrow = 2))
#' x$labels
#' 
#' # ========
#' # = Plot =
#' # ========
#' 
#' plot(cfa1, means = FALSE, fixed = TRUE)
#' plot(cfa1, std = TRUE, digits = 3, resid= 'line')
#' 
#' 
#' m1 = umxRAM("play", data = c("A", "B", "C"),
#' 	umxPath(unique.pairs = c("A", "B", "C"))
#' )
#' 
#' # ==============================================
#' # = Inspecting model parameters and residuals. =
#' # ==============================================
#' 
#' # Show parameters, below .1, with label containing `x2' 
#' parameters(cfa1, "above", .5, pattern= "x2")
#' 
#' residuals(cfa1, suppress = .005)
#' 
#' # ==================================
#' # = Modifying and comparing models =
#' # ==================================
#' 
#' # Variable names in the Duncan data
#' dimnames = c("RespOccAsp", "RespEduAsp", "RespParAsp", "RespIQ", "RespSES",
#'              "FrndOccAsp", "FrndEduAsp", "FrndParAsp", "FrndIQ", "FrndSES")
#' # lower-triangle of correlations among these variables
#' tmp = c(
#' 	0.6247,
#' 	0.2137, 0.2742,
#' 	0.4105, 0.4043, 0.1839,
#' 	0.3240, 0.4047, 0.0489, 0.2220,
#' 	0.3269, 0.3669, 0.1124, 0.2903, 0.3054,
#' 	0.4216, 0.3275, 0.0839, 0.2598, 0.2786, 0.6404,
#' 	0.0760, 0.0702, 0.1147, 0.1021, 0.0931, 0.2784, 0.1988, 
#' 	0.2995, 0.2863, 0.0782, 0.3355, 0.2302, 0.5191, 0.5007,  0.2087,
#' 	0.2930, 0.2407, 0.0186, 0.1861, 0.2707, 0.4105, 0.3607, -0.0438, 0.2950
#' )
#' 
#' # Use the umx_lower2full function to create a full correlation matrix
#' duncanCov = umx_lower2full(tmp, diag = FALSE, dimnames = dimnames)
#' 
#' # Turn the duncan data into an mxData object for the model
#' duncanCov = mxData(duncanCov, type = "cov", numObs = 300)
#' 
#' respondentFormants   = c("RespSES", "FrndSES", "RespIQ", "RespParAsp")
#' friendFormants       = c("FrndSES", "RespSES", "FrndIQ", "FrndParAsp")
#' latentAspiration     = c("RespLatentAsp", "FrndLatentAsp")
#' respondentOutcomeAsp = c("RespOccAsp", "RespEduAsp")
#' friendOutcomeAsp     = c("FrndOccAsp", "FrndEduAsp")
#' 
#' 
#' duncan1 = umxRAM("Duncan", data = duncanCov,
#' 	# Working from the left of the model, as laid out in the figure, to right...
#' 
#' 	# 1. Add all distinct paths between variables to allow the 
#' 	# exogenous manifests to covary with each other.
#' 	umxPath(unique.bivariate = c(friendFormants, respondentFormants)),
#' 
#' 	# 2. Add variances for the exogenous manifests,
#' 	# These are assumed to be error-free in this model,
#' 	# and are fixed at their known value).
#' 	umxPath(var = c(friendFormants, respondentFormants), fixedAt = 1),
#' 
#' 	# 3. Paths from IQ, SES, and parental aspiration 
#' 	# to latent aspiration for Respondents:
#' 	umxPath(respondentFormants, to = "RespLatentAsp"),
#' 	# And same for friends
#' 	umxPath(friendFormants,     to = "FrndLatentAsp"),
#' 
#' 	# 4. Add residual variance for the two aspiration latent traits.
#' 	umxPath(var = latentAspiration),
#' 
#' 	# 5. Allow the latent traits each influence the other.
#' 	# This is done using fromEach, and the values are 
#' 	# bounded to improve stability.
#' 	# note: Using one-label would equate these 2 influences
#' 	umxPath(fromEach = latentAspiration, lbound = 0, ubound = 1), 
#' 
#' 	# 6. Allow latent aspiration to affect respondent's
#' 	# occupational & educational aspiration.
#' 	# note: firstAt = 1 is used to provide scale to the latent variables.
#' 	umxPath("RespLatentAsp", to = respondentOutcomeAsp, firstAt = 1),
#' 
#' 	# And their friends
#' 	umxPath("FrndLatentAsp", to = friendOutcomeAsp, firstAt = 1),
#' 	
#' 	# 7. Finally, on the right hand side of figure, we add
#' 	# residual variance for the endogenous manifests.
#' 	umxPath(var = c(respondentOutcomeAsp, friendOutcomeAsp))
#' )
#' 
#' # ====================
#' # = Modifying models =
#' # ====================
#' 
#' # Collect a list of paths to drop
#' pathList = c("RespLatentAsp_to_FrndLatentAsp", "FrndLatentAsp_to_RespLatentAsp")
#' 
#' # Modify the model duncan1, requesting a comparison table:
#' duncan2 = umxModify(duncan1, update = pathList, name = "No_influence", comparison = TRUE)
#' 
#' # An example using regex, to drop all paths beginning "G_to_"
#' cfa2 = umxModify(cfa1, regex = "^G_to.*")
#' 
#' # ====================
#' # = Comparing models =
#' # ====================
#' 
#' umxCompare(duncan1, duncan2, report = "inline")
#' 
#' # To open the output as an html table in a browser, say:
#' umxCompare(duncan1, duncan2, report = "html")
#' 
#' # =============================
#' # = Equating model parameters =
#' # =============================
#' 
#' parameters(duncan1, pattern = "IQ_to_")
#' 
#' 
#' duncan3 = umxModify(duncan1, name = "Equate IQ effect", comparison = TRUE,
#' 	master = "RespIQ_to_RespLatentAsp", 
#' 	update = "FrndIQ_to_FrndLatentAsp" 
#' )
#' 
#' # ================
#' # = ACE examples =
#' # ================
#' 
#' require(umx); 
#' # open the built in dataset of Australian height and weight twin data
#' data("twinData")
#' selDVs = c("wt")
#' dz = twinData[twinData$zygosity == "DZFF", ]
#' mz = twinData[twinData$zygosity == "MZFF", ]
#' 
#' ACE1 = umxACE(selDVs = selDVs, dzData = dz, mzData = mz, sep = "")
#' ACE2 = umxModify(ACE1, update = "c_r1c1", name = "dropC")
#' umxSummary(ACE1, std = FALSE, report = 'html', digits = 3, comparison = ACE2)
#' parameters(ACE1)
#' 
#' ACE2 = umxModify(ACE1, update = "c_r1c1", name = "dropC")
#' 
#' # ================================
#' # = Example Common Pathway model =
#' # ================================
#' 
#' # load twin data built into umx
#' data("twinData")
#' 
#' # Selecting the 'ht' and 'wt' variables
#' selDVs = c("ht", "wt")
#' mzData = subset(twinData, zygosity == "MZFF",)
#' dzData = subset(twinData, zygosity == "DZFF",)
#' 
#' # Run and report a common-pathway model
#' CP1 = umxCP(selDVs = selDVs, dzData = dzData, mzData = mzData, suffix = "")
#' 
#' paths = c("c_cp_r1c1", "cs_r1c1", "cs_r2c2")
#' CP2 = umxModify(CP1, update = paths, name = "dropC", comparison = TRUE)
#' 
#' CP2 = umxModify(CP1, regex = "(^cs_)|(^c_cp_)", name = "dropC")
#' umxSummary(CP2, comparison = CP1)
#' 
#' # ====================================
#' # = Example Gene x environment model =
#' # ====================================
#' 
#' data("twinData")
#' twinData$age1 = twinData$age2 = twinData$age
#' # Define the DV and definition variables
#' selDVs  = c("bmi1", "bmi2")
#' selDefs = c("age1", "age2")
#' selVars = c(selDVs, selDefs)
#' 
#' # Create datasets
#' mzData  = subset(twinData, zygosity == "MZFF")
#' dzData  = subset(twinData, zygosity == "DZFF")
#' 
#' # Build, run and report the GxE model using selected DV and moderator
#' # umxGxE will remove and report rows with missing data in definition variables.
#' GE1 = umxGxE(selDVs = selDVs, selDefs = selDefs, 
#'   dzData = dzData, mzData = mzData,  dropMissingDef = TRUE)
#' 
#' # Shift the legend to the top right
#' umxSummary(GE1, location = "topright")
#' 
#' # plot standardized and raw output in separate graphs
#' umxSummary(GE1, separateGraphs = TRUE)
#' 
#' GE2 = umxModify(GE1, update = "am_r1c1", comparison = TRUE)
#' umxReduce(GE1)
#' 
#' # =================================
#' # = Example GxE windowed analysis =
#' # =================================
#' 
#' require(umx);
#' data("twinData") 
#' mod     = "age"
#' selDVs  = c("bmi1", "bmi2")
#' 
#' # select the younger cohort of twins
#' tmpTwin = twinData[twinData$cohort == "younger", ]
#' # Drop twins with missing moderator
#' tmpTwin = tmpTwin[!is.na(tmpTwin[mod]), ]
#' mzData  = subset(tmpTwin, zygosity == "MZFF", c(selDVs, mod))
#' dzData  = subset(tmpTwin, zygosity == "DZFF", c(selDVs, mod))
#' # toggle autoplot off, so we don't plot every level of the moderator
#' umx_set_auto_plot(FALSE)
#' umxGxE_window(selDVs = selDVs, moderator = mod, mzData = mzData, dzData = dzData)
#' umx_set_auto_plot(TRUE)
#' 
#' }
umxExamples <- function() {
	message("The example code used in our Twin Research and Human Genetics paper on umx is available by typing ?umxExamples")
}
