#' Extends Mendelian randomization with the twin design to test evidence of causality 
#'
#' @description
#' Testing causal claims is often difficult due to an inability to conduct experimental randomization of traits and 
#' situations to people. When twins are available, even when measured on a single occasion, the pattern of cross-twin
#' cross-trait correlations can (given distinguishable modes of inheritance for the two traits) falsify causal hypotheses.
#' 
#' `umxMRDoC` implements a 2-group model to form latent variables for each of two traits, and allows testing whether 
#' trait 1 causes trait 2, vice-versa, or even reciprocal causation. This is robust to several types of confounding 
#' due to the instrumental variable approach included in the model.
#'
#' This function applies both the MRDoC model and the MRDoC2 model depending on how many PRSs are passed as arguments.
#' 
#' @param data = NULL If building the MZ and DZ datasets internally from a complete data set.
#' @param pheno Phenotypes of interest, order matters ("exposure", "outcome")
#' @param prss Polygenic score(s). If a single one is passed MRDoC is run, MRDoC2 otherwise. 
#' @param mzData The MZ dataframe

#' @param dzData The DZ dataframe
#' @param name The name of the model (defaults to either "MRDoC" or "MRDoC2).
#' @param sibsData The unrelated sibs dataframe, requires "sibs" as an extra zygosity level.
#' @param sep The separator in twin variable names, default = "_T", e.g. "dep_T1".
#' @param autoRun Whether to run the model (default), or just to create it and return without running.
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "ordinal", "search".
#' @param optimizer Optionally set the optimizer (default NULL does nothing).
#' @param summary Optionally show a summary.
#' @param zygosity = "zygosity" (for the data= method of using this function).
#' @param covar = Covariates that will be regressed on X and Y phenotypes.
#' @param method Method for handling ordinal variables, defaults to Mehta.
#' @param verbose Outputs the pre-processing steps/warnings.
#' @param batteries Batteries included, currently scales continuous variables by default, "dump" will return data for inspection. Use NULL for disabling all.
#' @param sibs NEEDS DOCUMENTING (FALSE)
#' @param type Basic switch for estimation type. WLS tends to work really well in MRDoC, it saves you time. 
#' @return - [OpenMx::mxModel()] of subclass MxModelMRDoC
#' @export
#' @family Twin Modeling Functions
#' @seealso - [umxDoC()]
#' @references - Minica CC, Dolan CV, Boomsma DI, et al. (2018) Extending Causality Tests with Genetic Instruments: An Integration of Mendelian Randomization with the Classical Twin Design. Behavior Genetics 48(4): 337-349. \doi{10.1007/s10519-018-9904-4}
#' * McGue, M., Osler, M., & Christensen, K. (2010). Causal Inference and Observational Research: The Utility of Twins. *Perspectives on Psychological Science*, **5**, 546-556. \doi{10.1177/1745691610383511}
#' * Castro-de-Araujo LFS, Singh M, Zhou Y, et al. (2022) MR-DoC2: Bidirectional Causal Modeling with Instrumental Variables and Data from Relatives. Behavior Genetics. \doi{10.1007/s10519-022-10122-x}
#' @md
#' @examples
#' \dontrun{
#' 
#' # ================
#' # = 1. Load Data =
#' # ================
#' data(docData)
#' mzData  = subset(docData, zygosity %in% c("MZFF", "MZMM"))
#' dzData  = subset(docData, zygosity %in% c("DZFF", "DZMM"))
#'
#' # ============================
#' # = 2. Make a MRDoC2 model   =
#' # ============================
#' out = umxMRDoC(mzData = mzData, dzData = dzData,  
#'	pheno = c("varA1", "varA2"), prss = c("varB1", "varB2") )
#'}
umxMRDoC <- function(data = NULL, pheno, prss = NULL, mzData = NULL, dzData = NULL, sibsData = NULL,  zygosity = "zygosity", sep = "_T", summary = !umx_set_silent(silent = TRUE), name = NULL, autoRun = getOption("umx_auto_run"), sibs = FALSE, type = "FIML", tryHard = c("no", "yes", "ordinal", "search"), optimizer = NULL,  covar = NULL, batteries = c("scale"), method = "Mehta", verbose = FALSE) {

	#umx_set_silent(silent = TRUE)
	tryHard = match.arg(tryHard)
	sketch = FALSE
	indVar = NULL
	ordinalPresent = FALSE

		colTypes = NULL
		colTypes$nOrdVars = 0
		if (missing(data) && missing(mzData) && missing(dzData)) sketch = TRUE

		if (!is.null(covar)) {
			nCov   <- length(covar)
			indVar <- paste(covar,c(rep(1,nCov),rep(2,nCov)),sep=sep)
		}

	  if ("scale" %in% batteries && !sketch && !is.null(data)) {

			data = umx_scale_wide_twin_data(data, varsToScale = c(pheno, prss), sep = sep, twins = 1:2)

  		if (verbose) message("Scaling all continuous data")
			if ("dump" %in% batteries) return(data)
		}

		# Managing data
		if (!is.null(data)) {
			if ("tbl" %in% class(data)) {
				data = as.data.frame(data)
			}
			mzData = data[data[, zygosity] %in% ifelse(is.null(mzData), "MZ", mzData), ]
			dzData = data[data[, zygosity] %in% ifelse(is.null(dzData), "DZ", dzData), ]
			if (sibs) sibsData = data[data[, zygosity] %in% ifelse(is.null(sibsData), "sibs", sibsData), ]

			} else {
				if ("tbl" %in% class(mzData)) {
					mzData = as.data.frame(mzData)
					dzData = as.data.frame(dzData)
					if (sibs) sibsData = as.data.frame(sibsData)
				}
			}

			vnames = tvars(c(pheno, prss), sep = sep)

			if (!sketch){
				xmu_twin_check(
					selDVs = c(pheno, prss),
					sep = sep, dzData = dzData, mzData = mzData, enforceSep = TRUE,
					nSib = 2, optimizer = optimizer
					)

		# Find ordinal variables
		colTypes = umx_is_ordered(xmu_extract_column(mzData, vnames),
			summaryObject= TRUE)

		if (colTypes$nOrdVars > 0){
			ty = umxThresholdMatrix( rbind(mzData,dzData), fullVarNames = colTypes$ordVarNames,
				sep = sep, method=method)
			if (sibs) {
				ty = umxThresholdMatrix( rbind(mzData,dzData, sibsData), fullVarNames = colTypes$ordVarNames,
					sep = sep, method=method)
			}
			ordinalPresent = TRUE
		}

		if (!missing(mzData)) {mzData = xmu_make_mxData(mzData, manifests = vnames, fullCovs = indVar)}
		if (!missing(dzData)) {dzData = xmu_make_mxData(dzData, manifests = vnames, fullCovs = indVar)}
		if (!missing(sibsData)) {sibsData = xmu_make_mxData(sibsData, manifests = vnames, fullCovs = indVar)}
	}


  if (type == "WLS") {fit = mxFitFunctionWLS() } else {fit = mxFitFunctionML()}

	if (length(prss) == 1) {
		if (is.null(name)) { name = "MRDoC"}

		top <- mxModel("top",
			umxMatrix("filter",
				type = "Full", nrow = 5, ncol = 6, free = FALSE, byrow = TRUE,
				values = c(
					1, 0, 0, 0, 0, 0,
					0, 1, 0, 0, 0, 0,
					0, 0, 1, 0, 0, 0,
					0, 0, 0, 1, 0, 0,
					0, 0, 0, 0, 1, 0
					)
				),
			mxMatrix(name = "I", type = "Iden", nrow = 3, ncol = 3),
			umxMatrixFree("B",
				type = "Full", nrow = 3, ncol = 3,
				labels = c(
					NA, "g2", "b1",
					"g1", NA, "b2",
					NA, NA, NA
					),
				values = c(
					0, .1, .11,
					.2, 0, .15,
					0, 0, 0
					)
				),
			umxMatrix("psi_a", type = "Symm", ncol = 3, nrow = 3, byrow = TRUE,
				labels = c(
					"ax2",
					"covA", "ay2",
					NA, NA, "prsx2"
					),
				free = c(
					T,
					T, T,
					F, F, F
					),
				values = c(
					0.1,
					0.05, 0.1,
					0, 0, 1
					),
				),
			umxMatrixFree("psi_c", type = "Symm", ncol = 3, nrow = 3,
				labels = c(
					"cx2",
					"covC", "cy2",
					NA, NA, NA
					),values = c(
					0.1,
					0.05, 0.1,
					0, 0, 0
					),
					),
			umxMatrixFree("psi_e", type = "Symm", ncol = 3, nrow = 3,
				labels = c(
					"ex2",
					"covE", "ey2",
					NA, NA, NA
					),values = c(
					0.1,
					0.05, 0.1,
					0, 0, 0
					),
					),
			umxMatrix("lamba",
				type = "Diag", nrow = 3, ncol = 3, byrow = TRUE,
				free = c(FALSE, FALSE, TRUE), labels = c("σ1", "σ2", "σ3"),
				values = c(1, 1, 1)
				),
			mxAlgebra('solvBE'  , expression =  solve(I - B) ),
			mxAlgebra("A", expression = lamba %&% solvBE %&% psi_a),
			mxAlgebra("C", expression = lamba %&% solvBE %&% psi_c),
			mxAlgebra("E", expression = lamba %&% solvBE %&% psi_e),
			mxAlgebra("CovMZ", expression = filter %&% rbind(
				cbind(A + C + E, A + C),
				cbind(A + C, A + C + E)
				)),
			mxAlgebra("CovDZ", expression = rbind(
				cbind(A + C + E, 0.5 %x% A + C),
				cbind(0.5 %x% A + C, A + C + E)
				)),
			mxAlgebra("VC", expression = cbind(A, C, E, A / (A + C + E),
				C / (A + C + E),
				E / (A + C + E)),
			dimnames = list(
				rep("VC", 3),
				rep(c("A", "C", "E", "SA", "SC", "SE"), each = 3)
				)
			),
			# create algebra for expected mean & threshold matrices
			mxMatrix( type="Full", nrow=1, ncol=3, free= TRUE,
				labels=c("mean_Ph1","mean_Ph2","mean_prs"),
				name="meanT1" ),
			# in mz twins, prs_twin1 == prs_twin2
			# so, twin2 in mz pairs does not have the prs variable
			mxMatrix( type="Full", nrow=1, ncol=2, free=TRUE,
				labels=c("mean_Ph1","mean_Ph2"),
				name="meanT2MZ"),
			# in dz twins, prs_twin1 != prs_twin2
			mxMatrix( type="Full", nrow=1, ncol=3, free=TRUE,
				labels=c("mean_Ph1","mean_Ph2","mean_prs"),
				name="meanT2DZ" )
			)

		expMeanMZ <- mxAlgebra("expMeanMZ",
			expression = cbind(top.meanT1 ,
				top.meanT2MZ ))

		expMeanDZ <- mxAlgebra("expMeanDZ",
			expression = cbind(top.meanT1 ,
				top.meanT2DZ ))

		expMZ = mxExpectationNormal("top.CovMZ",
			means = "expMeanMZ", vnames[1:5])
		expDZ =  mxExpectationNormal("top.CovDZ",
			means =  "expMeanDZ", vnames)

		if (sibs){

			Ts <-  mxMatrix(name = 'Ts', type='Symm', nrow=3, ncol = 3,byrow = TRUE,
				labels =c("tx2", "covT",NA,
					"covT","ty2" ,NA,
					NA,    NA,    NA),
				values = c(.4, 0, 0,
					0,.4, 0,
					0,0,0),
				free= c(TRUE,TRUE,  FALSE,
					TRUE, TRUE, FALSE,
					FALSE,FALSE,FALSE),
				dimnames = list(c("X", "Y", "iX"),
					c("X", "Y", "iX")))

			expMeanSib <- mxAlgebra("expMeanSib",
				expression = cbind(top.meanT1 ,
					top.meanT2sib ))

			expSib =  mxExpectationNormal("top.CovSib",
				means =  "expMeanSib", vnames)


			top <- mxModel(top, Ts,
				mxMatrix( name="K", type="Full", nrow=1, ncol=1, free=FALSE, values=0.5, labels="k"),
				mxAlgebra('Ts_'  , expression = lamba %&% solvBE %&% Ts),
				mxAlgebra('CovMZ_', expression=rbind(
					cbind(A+C+E+Ts_, A + C+ Ts_),
					cbind(A + C+ Ts_, A+C+E+Ts_))),
				mxAlgebra('CovDZ', expression=rbind(
					cbind(A+C+E+Ts_, K%x%A + C + Ts_),
					cbind(K%x%A + C + Ts_, A+C+E+Ts_))),
				mxAlgebra('CovSib', expression=rbind(
					cbind(A+C+E+Ts_, K%x%A + C),
					cbind(K%x%A + C, A+C+E+Ts_))),
				mxAlgebra('CovMZ', expression= filter%&%CovMZ_),
				mxMatrix(name ="meanT2sib", type="Full", nrow=1, ncol=3, free=TRUE,
					labels=c("mean_Ph1","mean_Ph2","mean_prs")))
		}

		if(colTypes$nOrdVars > 0){
			expMZ =  mxExpectationNormal("top.CovMZ",
				means = "expMeanMZ",
				dimnames = vnames[1:5],
				thresholds="top.threshMat",
				threshnames = colTypes$ordVarNames
				)

			expDZ =mxExpectationNormal("top.CovDZ",
				means =  "expMeanDZ",
				dimnames = vnames,
				thresholds="top.threshMat",
				threshnames = colTypes$ordVarNames
				)

			if (sibs) {
				expSib = mxExpectationNormal("top.CovSib",
					means = "expMeanSib",
					dimnames = vnames,
					thresholds="top.threshMat",
					threshnames = colTypes$ordVarNames
					)
			}
		}
    
		MZ = mxModel("MZ", expMeanMZ, expMZ, fit)
		DZ = mxModel("DZ", expMeanDZ, expDZ, fit)
		if (sibs) sibsM = mxModel("Sibs", expMeanSib, expSib, fit)

		if (ordinalPresent)   top = mxModel(top, ty)

		if (!sketch){
			MZ = mxModel(MZ, mzData)
			DZ = mxModel(DZ, dzData)
			if (sibs) sibsM = mxModel(sibsM, sibsData)
		}

		model = mxModel(name, top, MZ, DZ, mxFitFunctionMultigroup(c( "MZ","DZ") ))
		if (sibs) model = mxModel(name, top, MZ, DZ,sibsM, mxFitFunctionMultigroup(c( "MZ","DZ", "Sibs")) )

		model = umxModify(model,  update = c("g2",   "covE"), autoRun = F)

	 if (!sketch) model = mxAutoStart(model)

		# Means according to presence of definition vars
		if (!is.null(covar)) {
			expMeanMZ <- mxAlgebra("expMeanMZ",
				expression = cbind(top.meanT1 + t(bCov%*%dCov1),
					top.meanT2MZ + t(bCov[1:2,] %*%dCov2)))

			expMeanDZ <- mxAlgebra("expMeanDZ",
				expression = cbind(top.meanT1 + t(bCov%*%dCov1),
					top.meanT2DZ + t(bCov%*%dCov2)))

			if (sibs) {
				expMeanSib <- mxAlgebra("expMeanSib",
					expression = cbind(top.meanT1 + t(bCov%*%dCov1),
						top.meanT2sib + t(bCov%*%dCov2)))
			}

	## Matrix to hold definition variables for the regression
				# Twin 1
				dCov1 <- mxMatrix( type = "Full", nrow = nCov, ncol = 1, free = FALSE,
					labels = paste("data.",paste(covar,c(rep(1,nCov)),sep=sep),sep=""),
					name = "dCov1" )
				# Twin 2
				dCov2 <- mxMatrix( type = "Full", nrow = nCov, ncol = 1, free = FALSE,
					labels = paste("data.",paste(covar,c(rep(2,nCov)),sep=sep),sep=""),
					name = "dCov2" )

				# Matrix to hold betas for the covariates
				# row 1 = pheno 1 (smk)
				# row 2 = pheno 2 (cpg)
				# row 3 = prs (betas fixed at zero)
				bCov <- mxMatrix( type = "Full", nrow = 3, ncol = nCov,
					free = c(rep(T,nCov),              # X  - regressed on covariates
					 rep(T,nCov),                      # Y  - regressed on covariates
					 rep(F,nCov)),                     # PRS  - not regressed on any covariate (exogenous IV)
					values = c(rep(0,nCov*3)), byrow = T,
					labels = c(paste("bX",covar,sep="_"),
						paste("bY",covar,sep="_"),
						rep(NA,nCov)),
					name = "bCov" )

				model$MZ = mxModel(model$MZ, expMeanMZ, expMZ, fit,
					dCov1, dCov2, bCov)
				model$DZ = mxModel(model$DZ, expMeanDZ, expDZ, fit,
					dCov1, dCov2, bCov)
				if (sibs) {
					model$Sibs =  mxModel(model$Sibs, expMeanSib, expSib, fit,
						dCov1, dCov2, bCov)
				}
			}


			} else if (length(prss) == 2) {
				if (is.null(name)) { name = "MRDoC2"}

				top <- mxModel("top",
					umxMatrix("filter",
						type = "Full", nrow = 6, ncol = 8, free = FALSE, byrow = TRUE,
						values = c(
							1, 0, 0, 0, 0, 0, 0, 0,
							0, 1, 0, 0, 0, 0, 0, 0,
							0, 0, 1, 0, 0, 0, 0, 0,
							0, 0, 0, 1, 0, 0, 0, 0,
							0, 0, 0, 0, 1, 0, 0, 0,
							0, 0, 0, 0, 0, 1, 0, 0
							)
						),
					mxMatrix(name ="I", type = "Iden", nrow = 4, ncol = 4),
					umxMatrixFree("B",
						type = "Full", nrow = 4, ncol = 4,
						labels = c(
							NA, "g2", "b1", "b4",
							"g1", NA, "b2", "b3",
							NA, NA, NA, NA,
							NA, NA, NA, NA
							),
						values = c(
							0, 0.1, 0.1, 0.1,
							0.1, 0, 0.1, 0.1,
							0, 0, 0, 0,
							0, 0, 0, 0
							),
						),
					umxMatrix("psi_a", type = "Symm", ncol = 4, nrow = 4, byrow = TRUE,
						labels = c(
							"ax2",
							"covA", "ay2",
							NA, NA, "prsx2",
							NA, NA, "rf", "prsy2"
							),
						free = c(
							T,
							T, T,
							F, F, F,
							F, F, T, F
							),
						values = c(
							0.1,
							0.05, 0.1,
							0, 0, 1,
							0, 0, 0.05, 1
							),
						),
					umxMatrixFree("psi_c", type = "Symm", ncol = 4, nrow = 4,
						labels = c(
							"cx2",
							"covC", "cy2",
							NA, NA, NA,
							NA, NA, NA, NA
							),
						values = c(
							0.1,
							0.05, 0.1,
							0, 0, 0,
							0, 0, 0, 0
							),
						),
					umxMatrixFree("psi_e", type = "Symm", ncol = 4, nrow = 4, byrow = TRUE,
						labels = c(
							"ex2",
							"covE", "ey2",
							NA, NA, NA,
							NA, NA, NA, NA
							),
						values = c(
							0.1,
							0.05, 0.1,
							0, 0, 0,
							0, 0, 0, 0
							),
						),
					umxMatrix("lamba", type = "Diag", nrow = 4, ncol = 4,
						free = c(F, F, T, T), labels = c("σ1", "σ2", "σ3", "σ4"),
						values = c(1, 1, 1, 1)
						),
					mxAlgebra('solvBE'  , expression =  solve(I - B) ),
					mxAlgebra("A", expression = lamba %&% solvBE %&% psi_a),
					mxAlgebra("C", expression = lamba %&% solvBE %&% psi_c),
					mxAlgebra("E", expression = lamba %&% solvBE %&% psi_e),
					mxAlgebra("CovMZ", expression = filter %&% rbind(
						cbind(A + C + E, A + C),
						cbind(A + C, A + C + E)
						)),
					mxAlgebra(name = "CovDZ", expression = rbind(
						cbind(A + C + E, 0.5 %x% A + C),
						cbind(0.5 %x% A + C, A + C + E)
						)),
					mxAlgebra(
						name = "VC", expression = cbind(A, C, E, A / (A + C + E),
							C / (A + C + E), E / (A + C + E)),
						dimnames = list(
							rep("VC", 4),
							rep(c("A", "C", "E", "SA", "SC", "SE"), each = 4)
							)
						),
			# create algebra for expected mean & threshold matrices
			mxMatrix( name = "meanT1", type="Full", nrow=1, ncol=4, free= TRUE,
				labels=c("mean_Ph1","mean_Ph2","mean_prs1", "mean_prs2")),
			# in mz twins, prs_twin1 == prs_twin2
			# so, twin2 in mz pairs does not have the prs variable
			mxMatrix(name ="meanT2MZ", type="Full", nrow=1, ncol=2, free=TRUE,
				labels=c("mean_Ph1","mean_Ph2")),
			# in dz twins, prs_twin1 != prs_twin2
			mxMatrix(name ="meanT2DZ", type="Full", nrow=1, ncol=4, free=TRUE,
				labels=c("mean_Ph1","mean_Ph2","mean_prs1", "mean_prs2"))
			)

				expMeanMZ <- mxAlgebra("expMeanMZ",
					expression = cbind(top.meanT1 ,
						top.meanT2MZ ))

				expMeanDZ <- mxAlgebra("expMeanDZ",
					expression = cbind(top.meanT1 ,
						top.meanT2DZ ))

				expMZ =  mxExpectationNormal("top.CovMZ",
					means = "expMeanMZ", vnames[1:6])

				expDZ = mxExpectationNormal("top.CovDZ",
					means =  "expMeanDZ", vnames)

				if (sibs){
					Ts <-  mxMatrix(name = 'Ts', type='Symm', nrow=4, ncol = 4,byrow = TRUE,
						labels =c("tx2", "covT",NA, NA,
							"covT","ty2" ,NA, NA,
							NA,    NA,    NA, NA,
							NA,    NA,    NA,NA),
						values = c(.4, 0, 0, 0,
							0,.4, 0, 0,
							0,0, 0, 0,
							0,0,0, 0),
						free= c(TRUE,TRUE,  FALSE, FALSE,
							TRUE, TRUE, FALSE, FALSE,
							FALSE, FALSE, FALSE, FALSE,
							FALSE,FALSE,FALSE, FALSE),
						dimnames = list(c("X", "Y", "iX", "iY"),
							c("X", "Y", "iX", "iY")))

					expMeanSib <- mxAlgebra("expMeanSib",
						expression = cbind(top.meanT1 ,
							top.meanT2sib ))

					expSib =  mxExpectationNormal("top.CovSib",
						means =  "expMeanSib", vnames)

					top <- mxModel(top, Ts,
		mxMatrix( name="K", type="Full", nrow=1, ncol=1, free=FALSE, values=0.5, labels="k"), #still locally identified with freely estimated "k"
		mxAlgebra('Ts_'  , expression = lamba %&% solvBE %&% Ts),
		mxAlgebra('CovMZ_', expression=rbind(
			cbind(A+C+E+Ts_, A + C+ Ts_),
			cbind(A + C+ Ts_, A+C+E+Ts_))),
		mxAlgebra('CovDZ', expression=rbind(
			cbind(A+C+E+Ts_, K%x%A + C + Ts_),
			cbind(K%x%A + C + Ts_, A+C+E+Ts_))),
		mxAlgebra('CovSib', expression=rbind(
			cbind(A+C+E+Ts_, K%x%A + C),
			cbind(K%x%A + C, A+C+E+Ts_))),
		mxAlgebra('CovMZ', expression= filter%&%CovMZ_),
		mxMatrix(name ="meanT2sib", type="Full", nrow=1, ncol=4, free=TRUE,
			labels=c("mean_Ph1","mean_Ph2","mean_prs1","mean_prs2")))

				}


				if(colTypes$nOrdVars > 0){
					expMZ =  mxExpectationNormal("top.CovMZ",
						means = "expMeanMZ",
						dimnames = vnames[1:6],
						thresholds="top.threshMat",
						threshnames = colTypes$ordVarNames
						)

					expDZ =mxExpectationNormal("top.CovDZ",
						means =  "expMeanDZ",
						dimnames = vnames,
						thresholds="top.threshMat",
						threshnames = colTypes$ordVarNames
						)

					if (sibs) {
						expSib = mxExpectationNormal("top.CovSib",
							means = "expMeanSib",
							dimnames = vnames,
							thresholds="top.threshMat",
							threshnames = colTypes$ordVarNames
							)
					}
				}

				MZ = mxModel("MZ", expMeanMZ, expMZ,  fit)
				DZ = mxModel("DZ", expMeanDZ, expDZ,  fit)

				if (sibs) sibsM = mxModel("Sibs", expMeanSib, expSib, fit)

				if (ordinalPresent)   top = mxModel(top, ty)

				if (!sketch){
					MZ = mxModel(MZ, mzData)
					DZ = mxModel(DZ, dzData)
					if (sibs) sibsM = mxModel(sibsM, sibsData)
				}

				model = mxModel(name, top, MZ, DZ, mxFitFunctionMultigroup(c( "MZ","DZ") ))
				if (sibs) model = mxModel(name, top, MZ, DZ,sibsM, mxFitFunctionMultigroup(c( "MZ","DZ", "Sibs")) )

				model = umxModify(model,  update = c("b2", "b4"), autoRun = F)
			if (!sketch) model = mxAutoStart(model)

		# Means according to presence of definition vars
		if (!is.null(covar)) {
			expMeanMZ <- mxAlgebra("expMeanMZ",
				expression = cbind(top.meanT1 + t(bCov%*%dCov1),
					top.meanT2MZ + t(bCov[1:2,] %*% dCov2)))

			expMeanDZ <- mxAlgebra("expMeanDZ",
				expression = cbind(top.meanT1 + t(bCov%*%dCov1),
					top.meanT2DZ + t(bCov%*%dCov2)))
						## Matrix to hold definition variables for the regression
				# Twin 1
				dCov1 <- mxMatrix( type = "Full", nrow = nCov, ncol = 1, free = FALSE,
					labels = paste("data.",paste(covar,c(rep(1,nCov)),sep=sep),sep=""),
					name = "dCov1" )
				# Twin 2
				dCov2 <- mxMatrix( type = "Full", nrow = nCov, ncol = 1, free = FALSE,
					labels = paste("data.",paste(covar,c(rep(2,nCov)),sep=sep),sep=""),
					name = "dCov2" )

			# Matrix to hold betas for the covariates
			# row 1 = pheno 1 (smk)
			# row 2 = pheno 2 (cpg)
			# row 3 = prs (betas fixed at zero)
			bCov <- mxMatrix( type = "Full", nrow = 4, ncol = nCov,
					free = c(rep(T,nCov),                      # X  - regressed on covariates
					 rep(T,nCov),                      # Y  - regressed on covariates
					 rep(F,nCov),                     # PRS  - not regressed on any covariate (exogenous IV)
					 rep(F,nCov)),                     # PRS  - not regressed on any covariate (exogenous IV)
					values = c(rep(0,nCov*4)), byrow = T,
					labels = c(paste("bX",covar,sep="_"),
						paste("bY",covar,sep="_"),
						rep(NA,nCov),
						rep(NA,nCov)),
					name = "bCov" )

			model$MZ = mxModel(model$MZ, expMeanMZ, expMZ, fit,
				dCov1, dCov2, bCov)
			model$DZ = mxModel(model$DZ, expMeanDZ, expDZ, fit,
				dCov1, dCov2, bCov)
			if (sibs) {
				model$Sibs =  mxModel(model$Sibs, expMeanSib, expSib, fit,
					dCov1, dCov2, bCov)
			}
		}

		} else if (length(prss) == 0) {
			if (is.null(name)) { name = "DoC"}

			top = mxModel("top",
				mxMatrix(name = "I", type = "Iden", nrow = 2, ncol = 2),
				umxMatrixFree("B",
					type = "Full", nrow = 2, ncol = 2,
					labels = c(
						NA, "g2",
						"g1", NA
						)
					),
				umxMatrix("psi_a", type = "Symm", ncol = 2, nrow = 2, byrow = TRUE,
					labels = c(
						"ax2",
						"covA", "ay2"
						),
					free = c(
						T,
						T, T
						),
					values = c(
						0.1,
						0.05, 0.1
						),
					),
				umxMatrixFree("psi_c", type = "Symm", ncol = 2, nrow = 2,
					labels = c(
						"cx2",
						"covC", "cy2"
						),
					),
				umxMatrix("psi_e", type = "Symm", ncol = 2, nrow = 2,
					labels = c(
						"ex2",
						"covE", "ey2"
						),
					free = c(
						T,
						F, T
						),
					values = c(
						0.1,
						0, 0.1
						),
					),
				umxMatrix("lamba", type = "Diag", nrow = 2, ncol = 2, byrow = TRUE,
					free = c(F, F), labels = c("σ1", "σ2"),
					values = c(1, 1)
					),
				mxAlgebra('solvBE'  , expression =  solve(I - B) ),
				mxAlgebra("A", expression = lamba %&% solvBE %&% psi_a),
				mxAlgebra("C", expression = lamba %&% solvBE %&% psi_c),
				mxAlgebra("E", expression = lamba %&% solvBE %&% psi_e),
				mxAlgebra("CovMZ", expression =  rbind(
					cbind(A + C + E, A + C),
					cbind(A + C, A + C + E)
					)),
				mxAlgebra(name = "CovDZ", expression = rbind(
					cbind(A + C + E, 0.5 %x% A + C),
					cbind(0.5 %x% A + C, A + C + E)
					)),
				mxAlgebra("VC", expression = cbind(A, C, E, A / (A + C + E),
					C / (A + C + E), E / (A + C + E)),
				dimnames = list(
					rep("VC", 2),
					rep(c("A", "C", "E", "SA", "SC", "SE"), each = 2)
					)
				) ,
			# create algebra for expected mean & threshold matrices
			mxMatrix( type="Full", nrow=1, ncol=2, free= TRUE,
				labels=c("mean_Ph1","mean_Ph2"),
				name="meanT1" ),
			# in mz twins, prs_twin1 == prs_twin2
			# so, twin2 in mz pairs does not have the prs variable
			mxMatrix( type="Full", nrow=1, ncol=2, free=TRUE,
				labels=c("mean_Ph1","mean_Ph2"),
				name="meanT2MZ"),
			# in dz twins, prs_twin1 != prs_twin2
			mxMatrix( type="Full", nrow=1, ncol=2, free=TRUE,
				labels=c("mean_Ph1","mean_Ph2"),
				name="meanT2DZ" )
			)

			expMeanMZ <- mxAlgebra("expMeanMZ",
				expression = cbind(top.meanT1 ,
					top.meanT2MZ ))

			expMeanDZ <- mxAlgebra("expMeanDZ",
				expression = cbind(top.meanT1 ,
					top.meanT2DZ ))

			expMZ =  mxExpectationNormal("top.CovMZ",
				means = "expMeanMZ", vnames)

			expDZ = mxExpectationNormal("top.CovDZ",
				means =  "expMeanDZ", vnames)

			if (sibs){

				Ts <-  mxMatrix(name = 'Ts', type='Symm', nrow=2, ncol = 2,byrow = TRUE,
					labels =c("tx2", "covT",
						"covT","ty2" ),
					values = c(.4, 0,
						0,.4),
					free= c(TRUE,TRUE,
						TRUE, TRUE),
					dimnames = list(c("X", "Y"),
						c("X", "Y")))

				expMeanSib <- mxAlgebra("expMeanSib",
					expression = cbind(top.meanT1 ,
						top.meanT2sib ))

				expSib =  mxExpectationNormal("top.CovSib",
					means =  "expMeanSib", vnames)

				top <- mxModel(top, Ts,
		mxMatrix( name="K", type="Full", nrow=1, ncol=1, free=FALSE, values=0.5, labels="k"), #still locally identified with freely estimated "k"
		mxAlgebra('Ts_'  , expression = lamba %&% solvBE %&% Ts),
		mxAlgebra('CovMZ', expression=rbind(
			cbind(A+C+E+Ts_, A + C+ Ts_),
			cbind(A + C+ Ts_, A+C+E+Ts_))),
		mxAlgebra('CovDZ', expression=rbind(
			cbind(A+C+E+Ts_, 0.5%x%A + C + Ts_),
			cbind(0.5%x%A + C + Ts_, A+C+E+Ts_))),
		mxAlgebra('CovSib', expression=rbind(
			cbind(A+C+E+Ts_, K%x%A + C),
			cbind(K%x%A + C, A+C+E+Ts_))),
		mxMatrix(name ="meanT2sib", type="Full", nrow=1, ncol=2, free=TRUE,
			labels=c("mean_Ph1","mean_Ph2")))

			}


			if(colTypes$nOrdVars > 0){
				expMZ =  mxExpectationNormal("top.CovMZ",
					means = "expMeanMZ",
					dimnames = vnames,
					thresholds="top.threshMat",
					threshnames = colTypes$ordVarNames
					)

				expDZ =mxExpectationNormal("top.CovDZ",
					means =  "expMeanDZ",
					dimnames = vnames,
					thresholds="top.threshMat",
					threshnames = colTypes$ordVarNames
					)
				if (sibs) {
					expSib = mxExpectationNormal("top.CovSib",
						means = "expMeanSib",
						dimnames = vnames,
						thresholds="top.threshMat",
						threshnames = colTypes$ordVarNames
						)

				}
			}

			MZ = mxModel("MZ", expMeanMZ, expMZ, fit)
			DZ = mxModel("DZ", expMeanDZ, expDZ, fit)

			if (sibs) sibsM = mxModel("Sibs", expMeanSib, expSib, fit)
			if (ordinalPresent)   top = mxModel(top, ty)

			if (!sketch){
				MZ = mxModel(MZ, mzData)
				DZ = mxModel(DZ, dzData)
				if (sibs) sibsM = mxModel(sibsM, sibsData)
			}

			model = mxModel(name, top, MZ, DZ, mxFitFunctionMultigroup(c( "MZ","DZ") ))
			if (sibs) model = mxModel(name, top, MZ, DZ,sibsM, mxFitFunctionMultigroup(c( "MZ","DZ", "Sibs")) )

			model = umxModify(model,  update = c("g2",  "covE"), autoRun = F)
		if (!sketch) model = mxAutoStart(model)

		## Means according to presence of definition vars
		if (!is.null(covar)) {
			expMeanMZ <- mxAlgebra("expMeanMZ",
				expression = cbind(top.meanT1 + t(bCov%*%dCov1),
					top.meanT2MZ + t(bCov[1:2,] %*%dCov2)))

			expMeanDZ <- mxAlgebra("expMeanDZ",
				expression = cbind(top.meanT1 + t(bCov%*%dCov1),
					top.meanT2DZ + t(bCov%*%dCov2)))
						 ## Matrix to hold definition variables for the regression
				# Twin 1
				dCov1 <- mxMatrix( type = "Full", nrow = nCov, ncol = 1, free = FALSE,
					labels = paste("data.",paste(covar,c(rep(1,nCov)),sep=sep),sep=""),
					name = "dCov1" )
				# Twin 2
				dCov2 <- mxMatrix( type = "Full", nrow = nCov, ncol = 1, free = FALSE,
					labels = paste("data.",paste(covar,c(rep(2,nCov)),sep=sep),sep=""),
					name = "dCov2" )

			# Matrix to hold betas for the covariates
			# row 1 = pheno 1 (smk)
			# row 2 = pheno 2 (cpg)
			# row 3 = prs (betas fixed at zero)
			bCov <- mxMatrix( type = "Full", nrow = 2, ncol = nCov,
				free = c(rep(T,nCov),     # X  - regressed on covariates
				 rep(T,nCov)),            # Y  - regressed on covariates
				values = c(rep(0,nCov*2)), byrow = T,
				labels = c(paste("bX",covar,sep="_"),
					paste("bY",covar,sep="_")),
				name = "bCov" )

			model$MZ = mxModel(model$MZ, expMeanMZ, expMZ, fit,
				dCov1, dCov2, bCov)
			model$DZ = mxModel(model$DZ, expMeanDZ, expDZ, fit,
				dCov1, dCov2, bCov)
			if (sibs) {
				model$Sibs =  mxModel(model$Sibs, expMeanSib, expSib, fit,
					dCov1, dCov2, bCov)
			}
		}

		} else {
			stop("Only 1 or 2 PRSs are supported")
		}

	model = as(model, "MxModelMRDoC") # set class so that S3s dispatch e.g. plot()

	if (sketch == F){
		if (tryHard %in% c("yes", "ordinal")) model = mxOption(model, 'mvnRelEps', value= mxOption(model, 'mvnRelEps')*5)
		model  = xmu_safe_run_summary(model, autoRun = autoRun,  summary = summary, tryHard =  tryHard, intervals = T)
	}

	return(model)
}


#' Present the results of a Mendelian Randomization Direction of Causation Model in a table
#'
#' Summarizes a MR Direction of Causation model, as returned by [umxMRDoC()]
#'
#' @aliases umxSummary.MxModelMRDoC
#' @param model A fitted [umxDoC()] model to summarize
#' @param digits Round to how many digits (default = 2)
#' @param std Whether to show the standardized model (TRUE) (ignored: used extended = TRUE to get unstandardized)
#' @param CIs Confidence intervals (default FALSE)
#' @param comparison Run mxCompare on a comparison model (default NULL)
#' @param RMSEA_CI Optionally compute CI on RMSEA.
#' @param report Print tables to the console (as 'markdown'), or open in browser ('html')
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param ... Optional additional parameters
#' @return - nothing
#' @export
#' @family Summary functions
#' @seealso - \code{\link{umxDoC}()}, [plot()], [umxSummary()] work for DoC models.
#' @md
umxSummaryMRDoC <- function(model, digits = 2, std = TRUE, CIs = FALSE, comparison = NULL, RMSEA_CI = FALSE, report = c("markdown", "html"), file = getOption("umx_auto_plot"), ...) {

	if (CIs){ model = umxCI(model, run = "if necessary")}

	model_summary = summary(model, refModels = mxRefModels(model, run = TRUE))

	if (CIs == TRUE) {
		tmp = cbind(model_summary$parameters, model_summary$CI) 
		# print(tmp)
		tmp = tmp[, c(1,12, 6, 11,13)]
	} else {
		tmp = model_summary$parameters
		tmp[, c(1, 5:8)]
	}#

	umx_print(tmp, digits = digits, report =report, caption = paste0("Parameter loadings for model ", omxQuotes(model$name)), na.print = "", zero.print = "0", justify = "none")

	with(model_summary, {
		if(!is.finite(TLI)){
			TLI_OK = "OK"
		} else {
			if(TLI > .95) {
				TLI_OK = "OK"
			} else {
				TLI_OK = "bad"
			}
		}
		if(!is.finite(RMSEA)) {
			RMSEA_OK = "OK"
		} else {
			if(RMSEA < .06){
			RMSEA_OK = "OK"
			} else {
				RMSEA_OK = "bad"
			}
		}

		if(RMSEA_CI){
			RMSEA_CI = RMSEA(model_summary)$txt
		} else {
			RMSEA_CI = paste0("RMSEA = ", round(RMSEA, 3))
		}
			Chi = model_summary$fit
			ChiDoF = model_summary$degreesOfFreedom
			fitMsg = paste0("\nModel Fit: \u03C7\u00B2(", ChiDoF, ") = ", round(Chi, 2), # was A7
				# "Chi2(", ChiDoF, ") = ", round(Chi, 2), # was A7
				", p "      , umx_APA_pval(p, .001, 3, addComparison = TRUE),
				"; CFI = "  , round(CFI, 3),
				"; TLI = "  , round(TLI, 3),
				"; ", RMSEA_CI
			)
			message(fitMsg)
			if(TLI_OK   != "OK"){ message("TLI is worse than desired (>.95)") }
			if(RMSEA_OK != "OK"){ message("RMSEA is worse than desired (<.06)")}
	})

}

#' @export
umxSummary.MxModelMRDoC <- umxSummaryMRDoC
