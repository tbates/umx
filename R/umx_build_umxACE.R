# R/umx_build_umxACE.R

#' Build and run a 2-group Cholesky ACE twin model (univariate or multivariate)
#'
#' @description
#' Implementing a core task in twin modeling, umxACE models the genetic and environmental
#' structure of one or more phenotypes (measured variables) using the Cholesky ACE model
#' (Neale and Cardon, 1996).
#' 
#' Classical twin modeling uses the genetic and environmental differences 
#' among pairs of mono-zygotic (MZ) and di-zygotic (DZ) twins reared together.
#' 
#' `umxACE` implements a 2-group model to capture these data and represent the phenotypic variance as a sum of Additive genetic,
#' unique environmental (E) and, optionally, either common or shared-environment (C) or 
#' non-additive genetic effects (D).
#' 
#' The following figure shows the ACE model for one variable "x" as a path diagram:
#' 
#' \if{html}{\figure{ACEunivariate.png}{options: style="width: 50\%;" alt="Figure: ACE univariate.png"}}
#' \if{latex}{\figure{ACEunivariate.pdf}{options: width=7cm}}
#'
#' `umxACE` allows multivariate analyses, and this brings us to the Cholesky part of the model.
#' 
#' The Cholesky decomposition creates as many latent A (and C and E) latent variables as there are phenotypes, and, moving 
#' from left to right, decomposes the variance in each phenotype into successively restricted 
#' factors. The following figure shows the multivariate ACE model for three variables:
#' 
#' \if{html}{\figure{ACEmatrix.png}{options: style="width: 50\%;" alt="Figure: ACE matrix.png"}}
#' \if{latex}{\figure{ACEmatrix.pdf}{options: width=7cm}}
#'
#' In this ACE model of three phenotypes, the expected variance-covariance matrix of the original data
#' is the product of each lower Cholesky and its transform (i.e., `A = a %*% t(a)` summed for `A+C+E`.
#' 
#' This lower-triangle decomposition feature of the Cholesky yields a model which is certain to both
#' account for all the variance (with some restrictions) in the data and be solvable.
#' 
#' This figure also contains the key to understanding how to modify models that `umxACE` produces
#' using `umxModify()` to drop paths  by label like `"a_r1c1"`. **n.b.**: Read the "Matrices and Labels in ACE model" section in details below...
#' 
#' **NOTE**: Scroll down to details for how to use the function, a figure and multiple examples.
#' 
#' @details
#' \strong{Covariates}
#' umxACE handles covariates by modelling them in the means.
#' On the plus side, there is no distributional assumption for this method. A downside of this approach is that all 
#' covariates must be non-NA, thus dropping any rows where one or more covariates are missing.
#' This can waste data. See also [umx_residualize()]).
#'
#' \strong{Data Input}
#' The function flexibly accepts raw data, and also summary covariance data 
#' (in which case the user must also supple numbers of observations for the two input data sets).
#' 
#' The `type` parameter can select how you want the model data treated.
#' "FIML" is the normal treatment. "cov" and "cor" will turn raw data into cor data for analysis, or
#' check that you've provided cor data as input.
#' 
#' Types "WLS", "DWLS", and "ULS" will process raw data into WLS data of these types.
#' 
#' The default, "Auto" will treat data as the type they are provided as.
#' 
#' \strong{Ordinal Data}
#' In an important capability, the model transparently handles ordinal (binary or multi-level
#' ordered factor data) inputs, and can handle mixtures of continuous, binary, and ordinal
#' data in any combination. An experimental feature is under development to allow Tobit modeling. 
#' 
#' The function also supports weighting of individual data rows. In this case,
#' the model is estimated for each row individually, then each row likelihood
#' is multiplied by its weight, and these weighted likelihoods summed to form
#' the model-likelihood, which is to be minimized.
#' This feature is used in the non-linear GxE model functions.
#' 
#' **Additional features**
#' The umxACE function supports varying the DZ genetic association (defaulting to .5)
#' to allow exploring assortative mating effects, as well as varying the DZ \dQuote{C} factor
#' from 1 (the default for modeling family-level effects shared 100% by twins in a pair),
#' to .25 to model dominance effects.
#'
#' **Matrices and Labels in ACE model**
#' 
#' Matrices 'a', 'c', and 'e' contain the path loadings of the Cholesky ACE factor model.
#' 
#' So, labels relevant to modifying the model are of the form `"a_r1c1", "c_r1c1"` etc.
#'
#' Variables are in rows, and factors are in columns. So to drop the influence of factor 2 on variable 3, you would say:
#'
#' `m2 = umxModify(m1, update = "c_r3c2")`
#'	
#' Less commonly-modified matrices are the mean matrix `expMean`. This has 1 row, and the columns are laid out for 
#' each variable for twin 1, followed by each variable for twin 2.
#' 
#' So, in a model where the means for twin 1 and twin 2 had been equated (set = to T1), you 
#' could make them independent again with this script:
#'
#' `m1$top$expMean$labels[1, 4:6] = c("expMean_r1c4", "expMean_r1c5", "expMean_r1c6")`
#'
#' \emph{note}: Only one of C or D may be estimated simultaneously. This restriction reflects the lack
#' of degrees of freedom to simultaneously model C and D with only MZ and DZ twin pairs (Eaves et al. 1978, p267).
#' 
#' @param name The name of the model (defaults to"ACE").
#' @param selDVs The variables to include from the data: preferably, just "dep" not c("dep_T1", "dep_T2").
#' @param selCovs (optional) covariates to include from the data (do not include sep in names)
#' @param sep The separator in twin variable names, often "_T", e.g. "dep_T1". Simplifies selDVs.
#' @param dzData The DZ dataframe.
#' @param mzData The MZ dataframe.
#' @param type Analysis method one of c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS")
#' @param data If provided, dzData and mzData are treated as levels of zyg to select() MZ and DZ data sets (default = NULL)
#' @param zyg If data provided, this column is used to select rows by zygosity (Default = "zygosity")
#' @param allContinuousMethod "cumulants" or "marginals". Used in all-continuous WLS data to determine if a means model needed.
#' @param residualizeContinuousVars Not yet implemented.
#' @param autoRun Whether to run the model (default), or just to create it and return without running.
#' @param intervals Whether to run mxCI confidence intervals (default = FALSE)
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "ordinal", "search"
#' @param optimizer Optionally set the optimizer (default NULL does nothing).
#' @param nSib Number of siblings in a family (default - 2). "3" = extra sib.
#' @param dzAr The DZ genetic correlation (defaults to .5, vary to examine assortative mating).
#' @param dzCr The DZ "C" correlation (defaults to 1: set to .25 to make an ADE model).
#' @param numObsDZ Number of DZ twins: Set this if you input covariance data.
#' @param numObsMZ Number of MZ twins: Set this if you input covariance data.
#' @param weightVar If provided, a vector objective will be used to weight the data. (default = NULL).
#' @param equateMeans Whether to equate the means across twins (defaults to TRUE).
#' @param boundDiag Numeric lbound for diagonal of the a, c, and e matrices. Defaults to 0 since umx version 1.8
#' @param addStd Whether to add the algebras to compute a std model (defaults to TRUE).
#' @param addCI Whether to add intervals to compute CIs (defaults to TRUE).
#' @return - [OpenMx::mxModel()] of subclass mxModel.ACE
#' @export
#' @family Twin Modeling Functions
#' @seealso - [umxPlotACE()], [umxSummaryACE()], [power.ACE.test()], [umxModify()]
#' @references - Eaves, L. J., Last, K. A., Young, P. A., & Martin, N. G. (1978). Model-fitting approaches 
#' to the analysis of human behaviour. *Heredity*, **41**, 249-320. \doi{https://doi.org/10.1038/hdy.1978.101}
#' 
#' @examples
#' \donttest{
#' require(umx)
#' # ============================
#' # = How heritable is height? =
#' # ============================
#' 
#' # 1. Height in meters has a tiny variance, and this makes optimising hard.
#' #    We'll scale it by 10x to make the Optimizer's task easier.
#' data(twinData) # ?twinData from Australian twins.
#' twinData[, c("ht1", "ht2")] = twinData[, c("ht1", "ht2")] * 10
#'
#' # 2. Make mz & dz data.frames (no need to select variables: umx will do this)
#' mzData = twinData[twinData$zygosity %in% "MZFF", ]
#' dzData = twinData[twinData$zygosity %in% "DZFF", ]
#' 
#' # 3. Built & run the model, controlling for age in the means model
#' m1 = umxACE(selDVs = "ht", selCovs = "age", sep = "", dzData = dzData, mzData = mzData)
#'
#' # sidebar: umxACE figures out variable names using sep: 
#' #    e.g. selVars = "wt" + sep= "_T" -> "wt_T1" "wt_T2"
#' 
#' umxSummary(m1, std = FALSE) # un-standardized
#'
#' # tip 1: set report = "html" and umxSummary prints the table to your browser!
#' # tip 2: plot works for umx: Get a figure of the model and parameters
#' # plot(m1) # Also, look at the options for ?plot.MxModel.
#' 
#' # ===========================================
#' # = Test ADE, AE, CE, E, and generate table =
#' # ===========================================
#'
#' umxReduce(m1, report="html", silent= TRUE)
#'
#' # ============================
#' # = Model, with 2 covariates =
#' # ============================
#'
#' # Create another covariate: cohort
#' twinData$cohort1 = twinData$cohort2 =twinData$part
#' mzData = twinData[twinData$zygosity %in% "MZFF", ]
#' dzData = twinData[twinData$zygosity %in% "DZFF", ]
#'
#' # 1. def var approach
#' m2 = umxACE(selDVs = "ht", selCovs = c("age", "cohort"), sep = "", dzData = dzData, mzData = mzData)
#'
#' # 2. Residualized approach: remove height variance accounted-for by age.
#' FFdata = twinData[twinData$zygosity %in% c("MZFF", "DZFF"), ]
#' FFdata = umx_residualize("ht", "age", suffixes = 1:2, data = FFdata)
#' mzData = FFdata[FFdata$zygosity %in% "MZFF", ]
#' dzData = FFdata[FFdata$zygosity %in% "DZFF", ]
#' m3 = umxACE(selDVs = "ht", sep = "", dzData = dzData, mzData = mzData)
#'
#' # =============================================================
#' # = ADE: Evidence for dominance ? (DZ correlation set to .25) =
#' # =============================================================
#' m2 = umxACE(selDVs = "ht", sep = "", dzData = dzData, mzData = mzData, dzCr = .25)
#' umxCompare(m2, m1) # ADE is better
#' umxSummary(m2, comparison = m1) 
#' # nb: Although summary is smart enough to print d, the underlying 
#' #     matrices are still called a, c & e.
#'
#' # tip: try umxReduce(m1) to automatically build and compare ACE, ADE, AE, CE
#' # including conditional probabilities!
#'
#' # ===================================================
#' # = WLS example using diagonal weight least squares =
#' # ===================================================
#'
#' m3 = umxACE(selDVs = "ht", sep = "", dzData = dzData, mzData = mzData, 
#' 	type = "DWLS", allContinuousMethod='marginals'
#' )
#'
#'
#' # ==============================
#' # = Univariate model of weight =
#' # ==============================
#'
#' # Things to note:
#' 
#' # 1. Weight has a large variance, and this makes solution finding very hard.
#' # Here, we residualize the data for age, which also scales weight and height.
#'
#' data(twinData)
#' tmp = umx_residualize(c("wt", "ht"), cov = "age", suffixes= c(1, 2), data = twinData)
#' mzData = tmp[tmp$zygosity %in% "MZFF", ]
#' dzData = tmp[tmp$zygosity %in% "DZFF", ]
#' 
#' # tip: You might also want transform variables
#' # tmp = twinData$wt1[!is.na(twinData$wt1)]
#' # car::powerTransform(tmp, family="bcPower"); hist(tmp^-0.6848438)
#' # twinData$wt1 = twinData$wt1^-0.6848438
#' # twinData$wt2 = twinData$wt2^-0.6848438
#' 
#' # 4. note: the default boundDiag = 0 lower-bounds a, c, and e at 0.
#' #    Prevents mirror-solutions. If not desired: set boundDiag = NULL.
#'
#' m2 = umxACE(selDVs = "wt", dzData = dzData, mzData = mzData, sep = "", boundDiag = NULL)
#'
#' # A short cut (which is even shorter for "_T" twin data with "MZ"/"DZ" data in zygosity column is:
#' m1 = umxACE(selDVs = "wt", sep = "", data = twinData,
#' 	dzData = c("DZMM", "DZFF", "DZOS"), mzData = c("MZMM", "MZFF"))
#' # |   |   a1|c1 |   e1|
#' # |:--|----:|:--|----:|
#' # |wt | 0.93|.  | 0.38|
#'
#' # tip: umx_make_twin_data_nice() will make data into this nice format for you!
#' 
#' # ======================
#' # = MODEL MODIFICATION =
#' # ======================
#' # We can modify this model, e.g. test shared environment. 
#' # Set comparison to modify, and show effect in one step.
#' 
#' m2 = umxModify(m1, update = "c_r1c1", name = "no_C", comparison = TRUE)
#' #*tip* call umxModify(m1) with no parameters, and it will print the labels available to fix!
#' # nb: You can see parameters of any model with parameters(m1)
#'
#' # =========================================================
#' # = Well done! Now you can make modify twin models in umx =
#' # =========================================================
#'
#' # =====================================
#' # = Bivariate height and weight model =
#' # =====================================
#' data(twinData)
#' # We'll scale height (ht1 and ht2) and weight
#' twinData = umx_scale_wide_twin_data(data = twinData, varsToScale = c("ht", "wt"), sep = "")
#' mzData = twinData[twinData$zygosity %in% c("MZFF", "MZMM"),]
#' dzData = twinData[twinData$zygosity %in% c("DZFF", "DZMM", "DZOS"), ]
#' m1 = umxACE(selDVs = c("ht", "wt"), sep = '', dzData = dzData, mzData = mzData)
#' umxSummary(m1)
#'
#' # ===================
#' # = Ordinal example =
#' # ===================
#' 
#' # Prep data
#' require(umx)
#' data(twinData)
#' # Cut BMI column to form ordinal obesity variables
#' obLevels = c('normal', 'overweight', 'obese')
#' cuts = quantile(twinData[, "bmi1"], probs = c(.5, .2), na.rm = TRUE)
#' twinData$obese1=cut(twinData$bmi1, breaks=c(-Inf,cuts,Inf), labels=obLevels)
#' twinData$obese2=cut(twinData$bmi2, breaks=c(-Inf,cuts,Inf), labels=obLevels)
#' 
#' # Make the ordinal variables into umxFactors
#' ordDVs = c("obese1", "obese2")
#' twinData[, ordDVs] = umxFactor(twinData[, ordDVs])
#' 
#' mzData = twinData[twinData$zygosity %in% "MZFF", ]
#' dzData = twinData[twinData$zygosity %in% "DZFF", ]
#' 
#' # Model and summary!
#' m1 = umxACE(selDVs = "obese", dzData = dzData, mzData = mzData, sep = '')
#'
#' # And controlling age (otherwise manifests appearance as latent C)
#' m1 = umxACE(selDVs = "obese", selCov= "age", dzData = dzData, mzData = mzData, sep = '')
#' # umxSummary(m1)
#'
#' # ============================================
#' # = Bivariate continuous and ordinal example =
#' # ============================================
#' data(twinData)
#' twinData= umx_scale_wide_twin_data(data=twinData,varsToScale="wt",sep= "")
#' # Cut BMI column to form ordinal obesity variables
#' obLevels   = c('normal', 'overweight', 'obese')
#' cuts       = quantile(twinData[, "bmi1"], probs = c(.5, .2), na.rm = TRUE)
#' twinData$obese1=cut(twinData$bmi1,breaks=c(-Inf,cuts,Inf),labels=obLevels)
#' twinData$obese2=cut(twinData$bmi2,breaks=c(-Inf,cuts,Inf),labels=obLevels)
#' # Make the ordinal variables into mxFactors
#' ordDVs = c("obese1", "obese2")
#' twinData[, ordDVs] = umxFactor(twinData[, ordDVs])
#' mzData = twinData[twinData$zygosity %in% "MZFF",] 
#' dzData = twinData[twinData$zygosity %in% "DZFF",]
#' mzData = mzData[1:80,] # just top 80 so example runs in a couple of secs
#' dzData = dzData[1:80,]
#' m1 = umxACE(selDVs= c("wt","obese"), dzData= dzData, mzData= mzData, sep='')
#' 
#' # And controlling age
#' m1 = umxACE(selDVs = c("wt","obese"), selCov= "age", dzData = dzData, mzData = mzData, sep = '')
#'
#' # =======================================
#' # = Mixed continuous and binary example =
#' # =======================================
#' require(umx)
#' data(twinData)
#' twinData= umx_scale_wide_twin_data(data= twinData,varsToScale= "wt", sep="")
#' # Cut to form category of 20% obese subjects
#' # and make into mxFactors (ensure ordered is TRUE, and require levels)
#' obLevels   = c('normal', 'obese')
#' cuts       = quantile(twinData[, "bmi1"], probs = .2, na.rm = TRUE)
#' twinData$obese1= cut(twinData$bmi1, breaks=c(-Inf,cuts,Inf), labels=obLevels) 
#' twinData$obese2= cut(twinData$bmi2, breaks=c(-Inf,cuts,Inf), labels=obLevels) 
#' ordDVs = c("obese1", "obese2")
#' twinData[, ordDVs] = umxFactor(twinData[, ordDVs])
#' 
#' selDVs = c("wt", "obese")
#' mzData = twinData[twinData$zygosity %in% "MZFF",]
#' dzData = twinData[twinData$zygosity %in% "DZFF",]
#' m1 = umxACE(selDVs = selDVs, dzData = dzData, mzData = mzData, sep = '')
#' umxSummary(m1)
#'
#' # ==============
#' # = Two binary =
#' # ==============
#' require(umx)
#' data(twinData)
#' htLevels   = c('short', 'tall')
#' obLevels   = c('normal', 'obese')
#' cuts       = quantile(twinData[, "bmi1"], probs = .2, na.rm = TRUE)
#' twinData$obese1= cut(twinData$bmi1, breaks=c(-Inf,cuts,Inf), labels=obLevels) 
#' twinData$obese2= cut(twinData$bmi2, breaks=c(-Inf,cuts,Inf), labels=obLevels) 
#' ordDVs = c("obese1", "obese2")
#' twinData[, ordDVs] = umxFactor(twinData[, ordDVs])
#' 
#' twinData$short1 = cut(twinData$ht1, breaks=c(-Inf,1.6,Inf), labels=htLevels) 
#' twinData$short2 = cut(twinData$ht2, breaks=c(-Inf,1.6,Inf), labels=htLevels) 
#' ordDVs = c("short1", "short2")
#' twinData[, ordDVs] = umxFactor(twinData[, ordDVs])
#'
#' mzData = twinData[twinData$zygosity %in% "MZFF",]
#' dzData = twinData[twinData$zygosity %in% "DZFF",]
#' m1 = umxACE(selDVs = c("short", "obese"), dzData = dzData, mzData = mzData, sep = '')
#'
#' # # ===================================
#' # Example with covariance data only =
#' # ===================================
#'
#' require(umx)
#' data(twinData)
#' twinData= umx_scale_wide_twin_data(data=twinData, varsToScale= "wt", sep="")
#' selDVs = c("wt1", "wt2")
#' mz = cov(twinData[twinData$zygosity %in%  "MZFF", selDVs], use = "complete")
#' dz = cov(twinData[twinData$zygosity %in%  "DZFF", selDVs], use = "complete")
#' m1 = umxACE(selDVs=selDVs, dzData=dz, mzData=mz, numObsDZ=569, numObsMZ=351)
#' umxSummary(m1)
#' plot(m1)
#' }
#'
umxACE <- function(name = "ACE", selDVs, selCovs = NULL, dzData= NULL, mzData= NULL, sep = NULL, data = NULL, zyg = "zygosity", type = c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS"), numObsDZ = NULL, numObsMZ = NULL, boundDiag = 0, allContinuousMethod = c("cumulants", "marginals"), autoRun = getOption("umx_auto_run"), intervals = FALSE, tryHard = c("no", "yes", "ordinal", "search"), optimizer = NULL, residualizeContinuousVars = FALSE, nSib = 2, dzAr = .5, dzCr = 1, weightVar = NULL, equateMeans = TRUE, addStd = TRUE, addCI = TRUE) {
	tryHard = match.arg(tryHard)
	type    = match.arg(type)
	allContinuousMethod = match.arg(allContinuousMethod)
	if(residualizeContinuousVars){
		stop("residualizing (as opposed to modelling) continuous variables not implemented yet: just set to FALSE for now")
	}

	if(dzCr == .25 & (name == "ACE")){ name = "ADE" }

	# if data provided create twin files 
	if(!is.null(data)){
		if(is.null(sep)){ sep = "_T" }
		# Avoid ingesting tibbles
		if("tbl" %in% class(data)){
			data = as.data.frame(data)
		}
		mzData = data[data[,zyg] %in% ifelse(is.null(mzData), "DZ", mzData), ]
		dzData = data[data[,zyg] %in% ifelse(is.null(dzData), "DZ", dzData), ]
	}else{
		# avoid ingesting tibbles
		if("tbl" %in% class(mzData)){
			mzData = as.data.frame(mzData)
			dzData = as.data.frame(dzData)
		}
	}

	xmu_twin_check(selDVs= selDVs, sep = sep, dzData = dzData, mzData = mzData, enforceSep = FALSE, nSib = nSib, optimizer = optimizer)
		
	# nSib = 2, equateMeans = TRUE, verbose = verbose

	# New-style build-block: Expand var names if necessary and make the basic components of a twin model
	# full names passed in... gosh I wish I'd not allowed this early on...
	selVars = xmu_twin_upgrade_selDvs2SelVars(selDVs = selDVs, sep = sep, nSib= nSib)

	model = xmu_make_TwinSuperModel(name=name, mzData = mzData, dzData = dzData, selDVs = selDVs, selCovs= selCovs, sep = sep, type = type, allContinuousMethod = allContinuousMethod, numObsMZ = numObsMZ, numObsDZ = numObsDZ, nSib= nSib, equateMeans = equateMeans, weightVar = weightVar, bVector = FALSE, verbose= FALSE)
	tmp   = xmu_starts(mzData, dzData, selVars = selDVs, sep = sep, nSib = nSib, varForm = "Cholesky", equateMeans= equateMeans, SD= TRUE, divideBy = 3)
	nVar  = length(selVars)/nSib; # Number of dependent variables per **INDIVIDUAL** (so x2 per family)
	# Finish building top
	# Finish building top
	if(nSib==2){
		expCovMZ = mxAlgebra(rbind (cbind(ACE,  AC), cbind( AC, ACE)), dimnames = list(selVars, selVars), name = "expCovMZ")
		expCovDZ = mxAlgebra(rbind (cbind(ACE, hAC), cbind(hAC, ACE)), dimnames = list(selVars, selVars), name = "expCovDZ")
	} else if (nSib==3) {
		expCovMZ = mxAlgebra(name="expCovMZ", dimnames = list(selVars, selVars), rbind(
			cbind(ACE,  AC, hAC),
		    cbind(AC , ACE, hAC),
		    cbind(hAC, hAC, ACE))
		)
		expCovDZ = mxAlgebra(name= "expCovDZ", dimnames = list(selVars, selVars), rbind(
			cbind(ACE, hAC, hAC),
			cbind(hAC, ACE, hAC),
			cbind(hAC, hAC, ACE))
		)
	}else{
		stop("3 sibs is experimental, but ", nSib, "? ... Maybe come back in 2024, best tim :-)")
	}
	
	top = mxModel(model$top,
		# NB: "top" defines the algebra of the twin model, which MZ and DZ slave off of
		# it already has the means model and thresholds matrix added if necessary  - see "xmu_make_TwinSuperModel" above

		# Additive, Common, and Unique environmental paths				
		umxMatrix("a", type = "Lower", nrow = nVar, ncol = nVar, free = TRUE, values = tmp$varStarts, byrow = TRUE),
		umxMatrix("c", type = "Lower", nrow = nVar, ncol = nVar, free = TRUE, values = tmp$varStarts, byrow = TRUE),
		umxMatrix("e", type = "Lower", nrow = nVar, ncol = nVar, free = TRUE, values = tmp$varStarts, byrow = TRUE), 

		umxMatrix("dzAr", "Full", 1, 1, free = FALSE, values = dzAr),
		umxMatrix("dzCr", "Full", 1, 1, free = FALSE, values = dzCr),
		# Multiply by each path coefficient by its inverse to get variance component
		# Quadratic multiplication to add common_loadings
		mxAlgebra(name = "A", a %*% t(a)), # Additive genetic variance
		mxAlgebra(name = "C", c %*% t(c)), # Common environmental variance
		mxAlgebra(name = "E", e %*% t(e)), # Unique environmental variance
		mxAlgebra(name = "ACE", A+C+E),
		mxAlgebra(name = "AC" , A+C  ),
		mxAlgebra(name = "hAC", (dzAr %x% A) + (dzCr %x% C)),
		expCovMZ, expCovDZ
	)

	model = mxModel(model, top) 

	if(!is.null(boundDiag)){
		if(!is.numeric(boundDiag)){
			stop("boundDiag must be NULL, a value or a vector of values. You gave me a ", class(boundDiag))
		} else {				
			newLbound = model$top$matrices$a@lbound
			if(length(boundDiag) > 1 ){
				if(length(boundDiag) != length(diag(newLbound)) ){
					stop("Typically boundDiag is 1 digit: if more, must be size of diag(a)")
				}
			}
			diag(newLbound) = boundDiag; 
			model$top$a$lbound = newLbound
			model$top$c$lbound = newLbound
			model$top$e$lbound = newLbound
		}
	}
	if(addStd){
		newTop = mxModel(model$top,
			umxMatrix("I", "Iden", nVar, nVar), # nVar Identity matrix
			mxAlgebra(name = "Vtot", A + C+ E), # Total variance
			mxAlgebra(name = "SD", solve(sqrt(I * Vtot))), # total variance --> 1/SD
			mxAlgebra(name = "a_std", SD %*% a), # standardized a
			mxAlgebra(name = "c_std", SD %*% c), # standardized c
			mxAlgebra(name = "e_std", SD %*% e), # standardized e

			mxAlgebra(name = "A_std", SD %&% A), # standardized A
			mxAlgebra(name = "C_std", SD %&% C), # standardized C
			mxAlgebra(name = "E_std", SD %&% E)  # standardized E
		)
		model = mxModel(model, newTop)
	}
	if(addCI){
		if(addStd){
			model = mxModel(model, mxCI(c('top.a_std', 'top.c_std', 'top.e_std')))
		}else{
			model = mxModel(model, mxCI(c('top.a', 'top.c', 'top.e')))
		}
	}
	# Trundle through and make sure values with the same label have the same start value... means for instance.
	model = omxAssignFirstParameters(model)
	model = as(model, "MxModelACE") # set class so that S3 plot() dispatches
	model = xmu_safe_run_summary(model, autoRun = autoRun, tryHard = tryHard, std = TRUE, intervals = intervals)
	return(model)
} # end umxACE


#' Cholesky ACE twin model with mxFitFunctionGLM
#'
#' @description
#' RAM MZ/DZ Cholesky ACE for exponential-family phenotypes. Not a
#' patch on [umxACE()]: that function uses `mxExpectationNormal` and
#' `a %*% t(a)` algebras, which GLM cannot use.
#'
#' Additive and shared factors have variance 1. MZ uses one shared `a_i` and
#' (if `dzCr` is 1) one shared `c_i` per factor: two copies with correlation 1
#' are singular for Laplace. DZ has `a_i_T1` with `a_i_T2` covariance `dzAr`.
#' Unique `e` is always per twin. Loadings use umxACE labels `a_r{row}c{col}`.
#' Off-diagonal `e` is the GLM analogue of E covariances (shared unique /
#' measurement error). Family leftover is still independent given eta.
#' Unique environment is the `e` latent **and** the family leftover. `nAGQ` is 1.
#' Cholesky is `nVar`-general. MZ Laplace dimension is `4 * nVar` latents.
#'
#' **Bernoulli: fix e diagonal at 1.** Leftover (probit 1, logit \eqn{\pi^2/3})
#' already sets the observation scale. Univariate ACE still has three variance
#' components and only two twin correlations, so a free `e_ii` is not identified
#' (`e` went to 0 and `a` was eaten). For binomial items, `e_rjc_j` is fixed at
#' 1 (same idea as fixing unique scale in threshold ACE). Off-diagonal `e` stays
#' free so multivariate unique covariance is not forced to 0. Do **not** set
#' `e11` to leftover: that would double-count unique on eta. [umxSummaryACE_GLM()]
#' still uses `E = ee' + D` for shares. Poisson keeps a free `e` diagonal
#' (leftover is on the counts). Poisson ACE starts the Cholesky diagonals at
#' \eqn{a=\sqrt{0.8}}, \eqn{e=\sqrt{0.2}}, `c` at 0.2 (80/20 A/E on eta; C not pinned to the floor) so
#' the optimizer walks downhill from a genetic model. Free ACE diagonals have
#' `lbound = 1e-4` so the derivative stays alive at the floor.
#'
#' **Gamma and inverse Gaussian: not for ACE.** Inverse Gaussian is the hitting-time
#' law of Brownian motion with drift (reaction time, age of onset). Gamma is the
#' constant-CV amount / duration model. Both need trial-level data and, for a
#' process interpretation, structure on two parameters (mean and shape/lambda, or
#' drift and barrier with diffusion scale fixed). Twin ACE is one phenotype per
#' person, so those uses are out of scope. The wrapper still accepts the families:
#' leftover shape (Gamma) or lambda (inverse Gaussian) is fixed at 1 (`phi = 1/theta`)
#' unless you pass a numeric `theta`; a free `theta` label is rejected (collides
#' with `e`). That is not the cognitive-psych constraint of fixing diffusion
#' scale \eqn{\sigma}. Leave them in the too-hard box; use Poisson for counts and
#' threshold [umxACE()] for binary.
#'
#' **Binomial `trials=` is not implemented.** `mxFamily()` can take `trials` as a
#' raw-data column (`n` successes out of `n_i` trials per row). This wrapper does
#' not: there is no `trials` argument, and MZ/DZ data are subset to the phenotype
#' `_T1`/`_T2` columns, so any `n` columns are dropped. Binomial here is Bernoulli
#' (`n = 1`) only. Grouped or item-sum binomials with a dynamic `n` cannot be
#' wired. Most real binomial counts are overdispersed; there is no beta-binomial
#' (or other extra-binomial leftover) to absorb that, so users will keep using
#' liability [umxACE()] with more than one threshold, or a Gaussian for large
#' counts. Negative binomial on the counts is the overdispersed alternative when
#' there is no upper bound.
#'
#' **Hessian / standard errors stay off.** [umxRAM_GLM()] (no latents) yields finite
#' SEs. Twin ACE always has several latents. On univariate Poisson, `n = 50` pairs
#' gave a Hessian that was not positive definite and SEs all `NA` (optimizer status 6).
#' At `n = 200` the Hessian was positive definite and SEs were finite, but SEs on
#' `a` and `c` were about 1 on loadings of about 0.5, and status was still 6. Default
#' remains `Calculate Hessian` / `Standard Errors` = `No`. After a fitted model,
#' family-score SEs: `mxComputeOnce('fitfunction', 'information', 'meat')` then
#' `mxComputeStandardError()`. Poisson intercept matches `glm()`; Poisson ACE
#' intercept matches a pair bootstrap in tests.
#'
#' @param name model name (default "ACE_GLM")
#' @param selDVs base phenotype names (sep expands to `_T1`/`_T2`). Cholesky is `nVar`-general.
#' @param mzData MZ data.frame
#' @param dzData DZ data.frame
#' @param sep twin separator (default `"_T"`; must not be `""`)
#' @param family `stats::family` or `mxFamily()` for the phenotype (both twins). Poisson is the intended ACE family. Binomial is Bernoulli only: `trials=` and per-row `n` columns are not implemented (see Details). Scale-identified (`e` diagonal fixed at 1) but leftover eats heritability — use threshold [umxACE()] for binary. Gamma and inverse Gaussian are accepted but not supported for ACE (trial-level process models; see Details).
#' @param theta passed to `mxFamily()`. For Gamma, leftover shape; for inverse Gaussian, leftover lambda. Both fixed (default 1, `phi = 1`). A free-parameter label is rejected (collides with `e`). Not used for Poisson or binomial.
#' @param nAGQ must be 1 (more than one latent)
#' @param dzAr DZ additive genetic correlation (default 0.5)
#' @param dzCr DZ C correlation (default 1; 0.25 for ADE)
#' @param equateMeans equate T1/T2 intercepts (default TRUE)
#' @param autoRun whether to run
#' @param tryHard [xmu_safe_run_summary()] tryHard
#' @return supermodel of class `MxModelACE_GLM`
#' @family Twin Modeling Functions
#' @export
#' @seealso [umxACE()], [umxRAM_GLM()], [umxSummaryACE_GLM()], [umxPlotACE_GLM()]
#' @md
#' @examples
#' \dontrun{
#' set.seed(1)
#' n = 40
#' a = 0.4; c = 0.2; e = 0.5; b0 = 0.3
#' simPair = function(rA, n) {
#' 	A = rnorm(n)
#' 	C = rnorm(n)
#' 	E1 = rnorm(n)
#' 	E2 = rnorm(n)
#' 	A2 = rA * A + sqrt(1 - rA^2) * rnorm(n)
#' 	y1 = rpois(n, lambda = exp(b0 + a * A + c * C + e * E1))
#' 	y2 = rpois(n, lambda = exp(b0 + a * A2 + c * C + e * E2))
#' 	data.frame(y_T1 = y1, y_T2 = y2)
#' }
#' mzData = simPair(1, n)
#' dzData = simPair(0.5, n)
#' m1 = umxACE_GLM(selDVs = "y", mzData = mzData, dzData = dzData, sep = "_T")
#' }
umxACE_GLM <- function(name = "ACE_GLM", selDVs, mzData, dzData, sep = "_T", family = stats::poisson(), theta = NULL, nAGQ = 1L, dzAr = 0.5, dzCr = 1, equateMeans = TRUE, autoRun = getOption("umx_auto_run"), tryHard = c("no", "yes", "ordinal", "search")) {
	tryHard = match.arg(tryHard)
	mxFamilyFun = get0("mxFamily", envir = asNamespace("OpenMx"), inherits = FALSE, ifnotfound = NULL)
	if (is.null(mxFamilyFun)) {
		stop("umxACE_GLM needs OpenMx with mxFamily() / mxFitFunctionGLM()", call. = FALSE)
	}
	if (is.null(sep) || !nzchar(sep)) {
		stop("umxACE_GLM needs a non-empty sep (e.g. '_T'). umxTwinMaker path expansion cannot use sep=\"\".", call. = FALSE)
	}
	if (length(nAGQ) != 1 || is.na(nAGQ) || as.integer(nAGQ) != 1L) {
		stop("umxACE_GLM has several ACE latents. nAGQ must be 1 (Laplace).", call. = FALSE)
	}
	if (inherits(mzData, "tbl")) mzData = as.data.frame(mzData)
	if (inherits(dzData, "tbl")) dzData = as.data.frame(dzData)
	xmu_twin_check(selDVs = selDVs, sep = sep, dzData = dzData, mzData = mzData, enforceSep = FALSE, nSib = 2)
	selVars = xmu_twin_upgrade_selDvs2SelVars(selDVs = selDVs, sep = sep, nSib = 2)
	nVar = length(selVars) / 2
	if (nVar < 1) {
		stop("umxACE_GLM needs at least one phenotype.", call. = FALSE)
	}
	if (length(selDVs) == nVar) {
		bases = selDVs
	} else {
		bases = unique(namez(selVars, paste0(sep, "[12]$"), replacement = ""))
		if (length(bases) != nVar) stop("selDVs must be the base phenotype name(s)", call. = FALSE)
	}
	t1n = paste0(bases, sep, "1")
	t2n = paste0(bases, sep, "2")
	need = c(t1n, t2n)
	for (nm in need) {
		if (!nm %in% names(mzData)) stop("MZ data missing column ", omxQuotes(nm), call. = FALSE)
		if (!nm %in% names(dzData)) stop("DZ data missing column ", omxQuotes(nm), call. = FALSE)
	}
	mzUse = mzData[, need, drop = FALSE]
	dzUse = dzData[, need, drop = FALSE]

	# Family key (OpenMx names). Binomial: leftover already scales the observation
	# model, so e_ii is fixed at 1. Gamma / inverse Gaussian: leftover is a free
	# shape or lambda (phi = 1/theta) that collides with e; fix theta at 1 (phi = 1)
	# unless the user passes a numeric theta. Free theta labels are rejected.
	famKey = NA_character_
	if (is(family, "MxFamily")) {
		famKey = family@family
	} else if (inherits(family, "family")) {
		rawFam = tolower(family$family)
		if (startsWith(rawFam, "negative binomial")) {
			famKey = "negativebinomial"
		} else if (rawFam %in% c("inverse.gaussian", "inverse gaussian")) {
			famKey = "inversegaussian"
		} else {
			famKey = rawFam
		}
	}
	isBinom = identical(famKey, "binomial")
	isPoisson = identical(famKey, "poisson")
	isDispGLM = famKey %in% c("gamma", "inversegaussian")
	varLbound = 1e-4
	# Poisson: start on a genetic ridge (A 80% / E 20% of eta). Other families keep 0.2.
	if (isPoisson) {
		aDiagStart = sqrt(0.8)
		cDiagStart = 0.2
		eDiagStart = sqrt(0.2)
	} else {
		aDiagStart = 0.2
		cDiagStart = 0.2
		eDiagStart = 0.2
	}
	if (isDispGLM) {
		if (is.character(theta) && length(theta) == 1 && nzchar(theta)) {
			stop("umxACE_GLM: a free theta label collides with latent e. Pass a positive number (default 1, GLM phi = 1/theta) so leftover is fixed and e is free.", call. = FALSE)
		}
		if (is(family, "MxFamily") && nzchar(family@thetaLabel)) {
			stop("umxACE_GLM: a free theta label collides with latent e. Use a fixed Gamma shape / inverse-Gaussian lambda (default 1).", call. = FALSE)
		}
		if (is.null(theta)) theta = 1
	}

	famList = list()
	for (nm in need) {
		if (is(family, "MxFamily")) {
			fam = family
			fam@variable = nm
			if (isDispGLM) {
				fam@thetaLabel = ""
				if (is.numeric(theta) && length(theta) == 1 && is.finite(theta) && theta > 0) {
					fam@theta = as.numeric(theta)
				} else if (!(is.finite(fam@theta) && fam@theta > 0)) {
					fam@theta = 1
				}
			}
			famList[[nm]] = fam
		} else {
			famList[[nm]] = mxFamilyFun(nm, family, theta = theta)
		}
	}

	# MZ: shared a_i and c_i (two copies with cor=1 are singular for Laplace).
	# DZ: a_i_T1 with a_i_T2 = dzAr; c shared if dzCr==1 else two c with cor dzCr.
	# e is always per twin (Cholesky within person). Labels a_r{row}c{col} as umxACE.
	cholLab = function(pref, row, col) paste0(pref, "_r", row, "c", col)
	loadPath = function(from, to, lab, onDiag, fixEdiag = FALSE, diagStart = 0.2) {
		if (isTRUE(fixEdiag) && isTRUE(onDiag)) {
			umxPath(from, to, labels = lab, values = 1, free = FALSE)
		} else if (onDiag) {
			umxPath(from, to, labels = lab, values = diagStart, lbound = varLbound)
		} else {
			umxPath(from, to, labels = lab, values = 0)
		}
	}
	aMZ = paste0("a", 1:nVar)
	cMZ = paste0("c", 1:nVar)
	eT1 = paste0("e", 1:nVar, sep, "1")
	eT2 = paste0("e", 1:nVar, sep, "2")
	aT1 = paste0("a", 1:nVar, sep, "1")
	aT2 = paste0("a", 1:nVar, sep, "2")
	cT1 = paste0("c", 1:nVar, sep, "1")
	cT2 = paste0("c", 1:nVar, sep, "2")
	mzLats = c(aMZ, cMZ, eT1, eT2)
	if (abs(dzCr - 1) < 1e-12) {
		dzLats = c(aT1, aT2, cMZ, eT1, eT2)
	} else {
		dzLats = c(aT1, aT2, cT1, cT2, eT1, eT2)
	}
	meanLabsT1 = character(nVar)
	meanLabsT2 = character(nVar)
	for (j in 1:nVar) {
		if (equateMeans) {
			meanLabsT1[j] = paste0("one_to_", bases[j])
			meanLabsT2[j] = paste0("one_to_", bases[j])
		} else {
			meanLabsT1[j] = paste0("one_to_", t1n[j])
			meanLabsT2[j] = paste0("one_to_", t2n[j])
		}
	}
	mzPaths = list(umxPath(v1m0 = mzLats), umxPath("one", to = c(t1n, t2n), labels = c(meanLabsT1, meanLabsT2)))
	dzPaths = list(umxPath(v1m0 = dzLats), umxPath("one", to = c(t1n, t2n), labels = c(meanLabsT1, meanLabsT2)))
	for (i in 1:nVar) {
		for (j in i:nVar) {
			onDiag = (j == i)
			labA = cholLab("a", j, i)
			mzPaths[[length(mzPaths) + 1]] = loadPath(aMZ[i], t1n[j], labA, onDiag, diagStart = aDiagStart)
			mzPaths[[length(mzPaths) + 1]] = loadPath(aMZ[i], t2n[j], labA, onDiag, diagStart = aDiagStart)
			dzPaths[[length(dzPaths) + 1]] = loadPath(aT1[i], t1n[j], labA, onDiag, diagStart = aDiagStart)
			dzPaths[[length(dzPaths) + 1]] = loadPath(aT2[i], t2n[j], labA, onDiag, diagStart = aDiagStart)
			labC = cholLab("c", j, i)
			if (abs(dzCr - 1) < 1e-12) {
				mzPaths[[length(mzPaths) + 1]] = loadPath(cMZ[i], t1n[j], labC, onDiag, diagStart = cDiagStart)
				mzPaths[[length(mzPaths) + 1]] = loadPath(cMZ[i], t2n[j], labC, onDiag, diagStart = cDiagStart)
				dzPaths[[length(dzPaths) + 1]] = loadPath(cMZ[i], t1n[j], labC, onDiag, diagStart = cDiagStart)
				dzPaths[[length(dzPaths) + 1]] = loadPath(cMZ[i], t2n[j], labC, onDiag, diagStart = cDiagStart)
			} else {
				mzPaths[[length(mzPaths) + 1]] = loadPath(cMZ[i], t1n[j], labC, onDiag, diagStart = cDiagStart)
				mzPaths[[length(mzPaths) + 1]] = loadPath(cMZ[i], t2n[j], labC, onDiag, diagStart = cDiagStart)
				dzPaths[[length(dzPaths) + 1]] = loadPath(cT1[i], t1n[j], labC, onDiag, diagStart = cDiagStart)
				dzPaths[[length(dzPaths) + 1]] = loadPath(cT2[i], t2n[j], labC, onDiag, diagStart = cDiagStart)
			}
			labE = cholLab("e", j, i)
			mzPaths[[length(mzPaths) + 1]] = loadPath(eT1[i], t1n[j], labE, onDiag, fixEdiag = isBinom, diagStart = eDiagStart)
			mzPaths[[length(mzPaths) + 1]] = loadPath(eT2[i], t2n[j], labE, onDiag, fixEdiag = isBinom, diagStart = eDiagStart)
			dzPaths[[length(dzPaths) + 1]] = loadPath(eT1[i], t1n[j], labE, onDiag, fixEdiag = isBinom, diagStart = eDiagStart)
			dzPaths[[length(dzPaths) + 1]] = loadPath(eT2[i], t2n[j], labE, onDiag, fixEdiag = isBinom, diagStart = eDiagStart)
		}
		dzPaths[[length(dzPaths) + 1]] = umxPath(aT1[i], with = aT2[i], free = FALSE, values = dzAr)
		if (abs(dzCr - 1) >= 1e-12) {
			dzPaths[[length(dzPaths) + 1]] = umxPath(cT1[i], with = cT2[i], free = FALSE, values = dzCr)
		}
	}

	flattenPaths = function(pathList) {
		out = list()
		for (i in seq_along(pathList)) {
			p = pathList[[i]]
			if (is.list(p) && !is(p, "MxPath")) {
				for (k in seq_along(p)) {
					out[[length(out) + 1]] = p[[k]]
				}
			} else {
				out[[length(out) + 1]] = p
			}
		}
		out
	}
	mzPaths = flattenPaths(mzPaths)
	dzPaths = flattenPaths(dzPaths)
	MZ = do.call(umxRAM_GLM, c(list("MZ"), mzPaths, list(data = mzUse, families = famList, nAGQ = 1L, autoRun = FALSE, tryHard = "no")))
	DZ = do.call(umxRAM_GLM, c(list("DZ"), dzPaths, list(data = dzUse, families = famList, nAGQ = 1L, autoRun = FALSE, tryHard = "no")))
	MZ$fitfunction$nAGQ = 1L
	DZ$fitfunction$nAGQ = 1L
	MZ = mxOption(MZ, "Calculate Hessian", "No")
	DZ = mxOption(DZ, "Calculate Hessian", "No")
	MZ = mxOption(MZ, "Standard Errors", "No")
	DZ = mxOption(DZ, "Standard Errors", "No")
	model = umxSuperModel(name, MZ, DZ, autoRun = FALSE)
	model = mxOption(model, "Calculate Hessian", "No")
	model = mxOption(model, "Standard Errors", "No")
	model = omxAssignFirstParameters(model)
	model = as(model, "MxModelACE_GLM")
	model = xmu_safe_run_summary(model, autoRun = autoRun, tryHard = tryHard, std = FALSE, comparison = FALSE, refModels = FALSE)
	return(model)
}


#' Run a Cholesky with covariates that are random (in the expected covariance matrix)
#'
#' Often, researchers include covariates in 2-group Cholesky [umxACE()] twin models.
#' The umxACEcov 'random' option models the covariates in the expected covariance matrix, thus allowing
#' all data to be preserved. The downside is that this method has a strong assumption
#' of multivariate normality. Covariates like age, which are perfectly correlated in twins cannot be used.
#' Covariates like sex, which are ordinal, violate the normality assumption.
#' Binary and ordinal covariates like sex also violate the normality assumption. Which is most of the use cases :-(.
#'
#' The following figure shows how the ACE model with random covariates appears as a path diagram:
#' 
#' \if{html}{\figure{ACEcovVarianceModel.png}{options: style="width: 50\%;" alt="Figure: ACEcovVarianceModel.png"}}
#' \if{latex}{\figure{ACEcovVarianceModel.pdf}{options: width=7cm}}
#'
#' 
#' @param name The name of the model (defaults to"ACE").
#' @param selDVs The variables to include from the data (do not include sep).
#' @param selCovs The covariates to include from the data (do not include sep).
#' @param dzData The DZ dataframe.
#' @param mzData The MZ dataframe.
#' @param sep Separator text between basename for twin variable names. Often "_T".
#' Used to expand selDVs into full column names, i.e., "dep" --> c("dep_T1", "dep_T2").
#' @param type Analysis method one of c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS")
#' @param allContinuousMethod "cumulants" or "marginals". Used in all-continuous WLS data to determine if a means model needed.
#' @param dzAr The DZ genetic correlation (defaults to .5, vary to examine assortative mating).
#' @param dzCr The DZ "C" correlation (defaults to 1: set to .25 to make an ADE model).
#' @param addStd Whether to add the algebras to compute a std model (defaults to TRUE).
#' @param addCI Whether to add intervals to compute CIs (defaults to TRUE).
#' @param boundDiag = Whether to bound the diagonal of the a, c, and e matrices.
#' @param equateMeans Whether to equate the means across twins (defaults to TRUE).
#' @param bVector Whether to compute row-wise likelihoods (defaults to FALSE).
#' @param autoRun Whether to run the model (default), or just to create it and return without running.
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "ordinal", "search"
#' @param optimizer optionally set the optimizer. Default (NULL) does nothing.
#' @return - [OpenMx::mxModel()] of subclass mxModel.ACEcov
#' @export
#' @family Twin Modeling Functions
#' 
#' @references - Neale, M. C., & Martin, N. G. (1989). The effects of age, sex, 
#' and genotype on self-report drunkenness following a challenge dose of alcohol. 
#' *Behavior Genetics*, **19**, 63-78. \doi{10.1007/BF01065884}.
#' * Schwabe, I., Boomsma, D. I., Zeeuw, E. L., & Berg, S. M. (2015). A New Approach
#' to Handle Missing Covariate Data in Twin Research : With an Application to
#' Educational Achievement Data. *Behavior Genetics*, **46**, 583-95. \doi{10.1007/s10519-015-9771-1}.
#'
#' @examples
#' \dontrun{
#' # ============================================
#' # = BMI, can't use Age as a random covariate =
#' # ============================================
#' require(umx)
#' data(twinData)
#' # Replicate age to age1 & age2
#' twinData$age1 = twinData$age2 = twinData$age
#' mzData = subset(twinData, zygosity == "MZFF")
#' dzData = subset(twinData, zygosity == "DZFF")
#'
#' # =====================================================================
#' # = Trying to use identical var (like age) as a random cov is ILLEGAL =
#' # =====================================================================
#' m1 = umxACEcov(selDVs = "bmi", selCovs = "age", dzData = dzData, mzData = mzData, sep = "")
#'
#' # ========================================================
#' # = Use lm-based age residualisation approach instead =
#' # ========================================================
#'
#' resid_data = umx_residualize("bmi", "age", suffixes = 1:2, twinData)
#' mzData = subset(resid_data, zygosity == "MZFF")
#' dzData = subset(resid_data, zygosity == "DZFF")
#' m2     = umxACE("resid", selDVs = "bmi", dzData = dzData, mzData = mzData, sep = "")
#'
#' # Univariate BMI without covariate of age for comparison
#' mzData = subset(twinData, zygosity == "MZFF")
#' dzData = subset(twinData, zygosity == "DZFF")
#' m3 = umxACE("raw_bmi", selDVs = "bmi", dzData = dzData, mzData = mzData, sep = "")
#' 
#' # ===========================================================================
#' # = A bivariate example (need a dataset with a VIABLE COVARIATE to do this) =
#' # ===========================================================================
#' selDVs  = "wt" # Set the DVs
#' selCovs = "ht" # Set the COV
#' selVars = umx_paste_names(selDVs, covNames = selCovs, sep = "", suffixes=1:2)
#' mzData = subset(twinData, zygosity == "MZFF")
#' dzData = subset(twinData, zygosity == "DZFF")
#' m1 = umxACEcov(selDVs = selDVs, selCovs = selCovs,
#'    dzData = dzData, mzData = mzData, sep = "", autoRun = TRUE
#' )
#' }
umxACEcov <- function(name = "ACEcov", selDVs, selCovs, dzData, mzData, sep = NULL, type = c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS"), allContinuousMethod = c("cumulants", "marginals"), dzAr = .5, dzCr = 1, addStd = TRUE, addCI = TRUE, boundDiag = 0, equateMeans = TRUE, bVector = FALSE, autoRun = getOption("umx_auto_run"), tryHard = c("no", "yes", "ordinal", "search"), optimizer = NULL) {
	# TODO sub-class umxACEcov (random covariates) fn to support umxSummary and plot
	
	nSib = 2 # Number of siblings in a twin pair
	tryHard             = match.arg(tryHard)
	type                = match.arg(type)
	allContinuousMethod = match.arg(allContinuousMethod)
	
	if(type!="Auto"){
		stop("type not support in umxACEcov yet, sorry...")
	}

	if(!is.null(optimizer)){
		umx_set_optimizer(optimizer)
	}
	if(dzCr == .25 && name == "ACEcov"){
		name = "ADEcov"
	}

	# ==================
	# = Validate input =
	# ==================
	# Look for name conflicts
	badNames = umx_grep(selDVs, grepString = "^[ACDEacde][0-9]*$")
	if(!identical(character(0), badNames)){
		stop("The data contain variables that look like parts of the a, c, e model, i.e., a1 is illegal.\n",
		"BadNames included: ", omxQuotes(badNames) )
	}

	if(is.null(sep)){
		stop("I need a sep, like '_T'. (I will add 1 and 2 after it...) \n",
		"i.e., selDVs should be 'bmi' etc., and I will re-name to 'bmi_T1' and 'bmi_T2' etc.")
	}else if(length(sep) > 1){
			stop("sep should be just one word, like '_T'. I will add 1 and 2 after that...\n",
			"i.e., if variables are like 'var_T1', give me selVars = 'var' and sep = '_T'")
	}else{
		# stash base names for use later
		baseDVs  = selDVs
		baseCovs = selCovs
		# fill out full trait names
		selDVs  = umx_paste_names(baseDVs , sep = sep, suffixes = (1:nSib) )
		selCovs = umx_paste_names(baseCovs, sep = sep, suffixes = (1:nSib) )
	}

	nDV  = length(baseDVs)
	nCov = length(baseCovs)
	nVar = nDV + nCov # Number of variables per **INDIVIDUAL** ( so * 2 for a family)

	selVars = c(selDVs, selCovs)
	umx_check_names(selVars, mzData)
	umx_check_names(selVars, dzData)
	# message("selVars: ", omxQuotes(selVars))

	# Drop unused columns from mzData and dzData
	mzData = mzData[, selVars]
	dzData = dzData[, selVars]
	# check covariates are not identical across twins
	for (i in baseCovs) {
		checkVars = umx_paste_names(i , sep = sep, suffixes = (1:nSib) )
		if(cor(mzData[, checkVars], use = "com")[2, 1] == 1){
			stop("The covariate ", omxQuotes(i), " is identical for twin 1 and twin 2... That's not allowed for random-effects covariates. Try modeling this as a def var in the means instead.")
		}		
	}

	if(any(umx_is_ordered(dzData, strict = FALSE))){
		stop("Sorry: umxACEcov can't handle ordinal yet: e-mail tim and chew him out")
	}
	if(nrow(dzData) == 0){ stop("Your DZ dataset has no rows!") }
	if(nrow(mzData) == 0){ stop("Your MZ dataset has no rows!") }

	# ===============
	# = Setup means =
	# ===============
	# c(T1 DV means, T1 DV means, T1 COV means, T1 COV means)
	# Equate means for twin1 and twin 2 by matching labels in the first and second halves of the means labels matrix
	if(equateMeans){
		meanDimNames  = list(NULL, selVars)
		meanLabels    = umx_paste_names(varNames = baseDVs, covNames = baseCovs, suffixes = c('',''), prefix = "expMean_")
		DVmeanStarts  = umx_means(mzData[, selDVs[1:nDV]  , drop = FALSE], ordVar = 0, na.rm = TRUE)
		CovMeanStarts = umx_means(mzData[, selCovs[1:nCov], drop = FALSE], ordVar = 0, na.rm = TRUE)
		meanStarts    = c(DVmeanStarts, DVmeanStarts, CovMeanStarts, CovMeanStarts)
	} else {
		stop("Sorry, currently, means must be equated...")
	}
	
	# Make beta labels
	betaLabels = paste0(rep(paste0("var", 1:nDV), each = nCov), rep(paste0("beta", 1:nCov), times = nDV))
	betaLabels = matrix(betaLabels, nrow = nCov, ncol  = nDV, byrow = FALSE)

	# =====================
	# = Set up varStarts  =
	# =====================

	# DVS
	DVvarStarts = umx_var(mzData[, selDVs[1:nDV], drop = FALSE], format= "diag", ordVar = 1, use = "pairwise.complete.obs")
	if(nDV == 1){
		DVvarStarts = sqrt(DVvarStarts/3)
	} else {
		DVvarStarts = t(chol(diag(DVvarStarts/3))) # Divide variance up equally, and set to Cholesky form.
	}
	DVvarStarts = matrix(DVvarStarts, nDV, nDV)
	
	# Covariates
	covStarts = umx_var(mzData[, selCovs[1:nCov], drop = FALSE], format= "diag", ordVar = 1, use = "pairwise.complete.obs")
	if(nCov == 1){
		covStarts = sqrt(covStarts)
	} else {
		covStarts = t(chol(diag(covStarts))) # Set to Cholesky form.
	}
	covStarts = matrix(covStarts, nCov, nCov)
	
	
	top = mxModel("top",
		# "top" defines the algebra of the twin model, which MZ and DZ slave off of.
		umxMatrix("dzAr", "Full", 1, 1, free = FALSE, values = dzAr),
		umxMatrix("dzCr", "Full", 1, 1, free = FALSE, values = dzCr),

		# Matrices a, c, e to store a, c, e path coefficients.
		umxMatrix(name = "a", type = "Lower", nrow = nDV, ncol = nDV, free = TRUE, values = DVvarStarts, byrow = TRUE, dimnames = list(baseDVs, baseDVs)),
		umxMatrix(name = "c", type = "Lower", nrow = nDV, ncol = nDV, free = TRUE, values = DVvarStarts, byrow = TRUE),
		umxMatrix(name = "e", type = "Lower", nrow = nDV, ncol = nDV, free = TRUE, values = DVvarStarts, byrow = TRUE),  
		# Matrices A, C,E + compute variance components
		mxAlgebra(name = "A", a %*% t(a)),
		mxAlgebra(name = "C", c %*% t(c)),
		mxAlgebra(name = "E", e %*% t(e)),

		umxMatrix("expMean", "Full" , nrow = 1, ncol = (nVar * nSib), free = TRUE, values = meanStarts, labels = meanLabels, dimnames = meanDimNames),

		# general (between-pair) cov of covariates
		umxMatrix("lowerB", 'Lower', nrow = nCov, ncol = nCov, values = (covStarts * .4), free = TRUE),
		# # specific (within-pair) cov of covariates
		umxMatrix("lowerW", 'Lower', nrow = nCov, ncol = nCov, values = (covStarts * .6), free = TRUE),

		mxAlgebra(name= "CovB" , lowerB %*% t(lowerB)),
		mxAlgebra(name= "CovW" , lowerW %*% t(lowerW)),
		mxAlgebra(name= "CovWB", CovW + CovB),
		
		# ========================================
		# = Beta matrix of regression parameters =
		# ========================================
		mxMatrix(name = "beta", type = "Full", nrow = nCov, ncol  = nDV, free = TRUE, values = 0, labels = betaLabels),
		# Some handy component algebras
		mxAlgebra(name = "ACE", A + C + E),
		mxAlgebra(name = "AC" , A + C),
		mxAlgebra(name = "hAC", (dzAr %x% A) + (dzCr %x% C)),

		mxAlgebra(name = "bCovWBb", (t(beta) %*% CovWB) %*% beta), # output is[nDV * nDV] to match ACE
		mxAlgebra(name = "bCovBb" , (t(beta) %*% CovB)  %*% beta),
		mxAlgebra(name = "bCovWB" ,  t(beta) %*% CovWB),
		mxAlgebra(name = "bCovB"  ,  t(beta) %*% CovB),
		mxAlgebra(name = "CovWBb" ,              CovWB %*% beta),
		mxAlgebra(name = "CovBb"  ,               CovB %*% beta),

		# Algebra for expected variance/covariance matrix #in MZ twins
		mxAlgebra(name = "expCovMZ", dimnames = list(names(mzData), names(mzData)), expression = rbind(
			cbind(ACE + bCovWBb, AC  + bCovBb , bCovWB, bCovB),
			cbind(AC  + bCovBb , ACE + bCovWBb, bCovB , bCovWB),
			cbind(       CovWBb,        CovBb , CovWB , CovB),
			cbind(       CovBb ,        CovWBb, CovB  , CovWB))
		),

		# Algebra for expected variance/covariance matrix #in DZ twins
		mxAlgebra(name = "expCovDZ", dimnames = list(names(dzData), names(dzData)), expression = rbind(
			cbind(ACE + bCovWBb, hAC + bCovBb , bCovWB, bCovB),
			cbind(hAC + bCovBb , ACE + bCovWBb, bCovB , bCovWB),
			cbind(       CovWBb,        CovBb ,  CovWB,  CovB),
			cbind(       CovBb ,        CovWBb,  CovB  , CovWB))
		)
	) # end top

	MZ = mxModel("MZ",
		mxData(mzData , type = "raw"),
		mxExpectationNormal("top.expCovMZ", means= "top.expMean", dimnames = names(mzData)),
		mxFitFunctionML()
	)

	DZ = mxModel("DZ",
		mxData(dzData, type = "raw"),
		mxExpectationNormal("top.expCovDZ", means = "top.expMean", dimnames = names(dzData)),
		mxFitFunctionML()
	)
	
	model = mxModel(name, MZ, DZ, top,
		mxFitFunctionMultigroup(c("MZ", "DZ"))
	)
	if(!is.null(boundDiag)){
		if(!is.numeric(boundDiag)){
			stop("boundDiag must be a digit or vector of numbers. You gave me a ", class(boundDiag))
		} else {				
			newLbound = model$top$matrices$a@lbound
			if(length(boundDiag) > 1 ){
				if(length(boundDiag) != length(diag(newLbound)) ){
					stop("Typically boundDiag is 1 digit: if more, must be size of diag(a)")
				}
			}
			diag(newLbound) = boundDiag; 
			model$top$a$lbound = newLbound
			model$top$c$lbound = newLbound
			model$top$e$lbound = newLbound
		}
	}
	
	if(addStd){
		newTop = mxModel(model$top,
			mxMatrix(name  = "Iden", "Iden", nDV, nDV), # nDV Identity matrix
			mxAlgebra(name = "Vtot", A + C+ E),         # Total variance
			mxAlgebra(name = "SD", solve(sqrt(Iden * Vtot))), # Total variance
			mxAlgebra(name = "a_std", SD %*% a), # standardized a
			mxAlgebra(name = "c_std", SD %*% c), # standardized c
			mxAlgebra(name = "e_std", SD %*% e)  # standardized e
		)
		model = mxModel(model, newTop)
		if(addCI){
			model = mxModel(model, mxCI(c('top.a_std', 'top.c_std', 'top.e_std')))
		}
	}
	# Just trundle through and make sure values with the same label have the same start value... means for instance.
	model = omxAssignFirstParameters(model)
	model = as(model, "MxModelACEcov") # set class so umxSummary, plot, etc. work.
	model = xmu_safe_run_summary(model, autoRun = autoRun, tryHard = tryHard)
	invisible(model)
}
