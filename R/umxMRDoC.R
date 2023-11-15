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
#' @param name The name of the model (defaults to either "MRDoC" or "MRDoC2).
#' @param pheno Phenotypes of interest, order matters ("exposure", "outcome")
#' @param prss Polygenic score(s). If a single one is passed MRDoC is run, MRDoC2 otherwise. 
#' @param dzData The DZ dataframe
#' @param mzData The MZ dataframe
#' @param sep The separator in twin variable names, default = "_T", e.g. "dep_T1".
#' @param autoRun Whether to run the model (default), or just to create it and return without running.
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "ordinal", "search"
#' @param optimizer Optionally set the optimizer (default NULL does nothing).
#' @param summary Optionally show a summary.
#' @param data = NULL If building the MZ and DZ datasets internally from a complete data set.
#' @param zyg = "zygosity" (for the data= method of using this function)
#' @return - [mxModel()] of subclass MxModelMRDoC
#' @export
#' @family Twin Modeling Functions
#' @seealso - [umxDoC()]
#' @references - Minica CC, Dolan CV, Boomsma DI, et al. (2018) Extending Causality Tests with Genetic Instruments: An Integration of Mendelian Randomization with the Classical Twin Design. Behavior Genetics 48(4): 337–349. \doi{10.1007/s10519-018-9904-4}
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
#'	pheno = c("varA1", "varA2"), prss = c("varB1", "varA3") )
#'}
umxMRDoC <- function(pheno, prss, mzData = NULL, dzData = NULL, data = NULL, zyg = NULL, sep = "_T", summary = !umx_set_silent(silent = TRUE), name = NULL, autoRun = getOption("umx_auto_run"), tryHard = c("no", "yes", "ordinal", "search"), optimizer = NULL) {

  sketch = FALSE
  if (missing(data) && missing(mzData) && missing(dzData)) sketch = TRUE


  tryHard = match.arg(tryHard)

  # options(mxByrow = TRUE)
  umx_set_silent(TRUE)

  # Managing data
  if (!is.null(data)) {
    if ("tbl" %in% class(data)) {
      data = as.data.frame(data)
    }
    mzData = data[data[, zyg] %in% ifelse(is.null(mzData), "MZ", mzData), ]
    dzData = data[data[, zyg] %in% ifelse(is.null(dzData), "DZ", dzData), ]
  } else {
    if ("tbl" %in% class(mzData)) {
      mzData = as.data.frame(mzData)
      dzData = as.data.frame(dzData)
    }
  }

  vnames = tvars(c(pheno, prss), sep = sep)

  if (sketch==F){
    xmu_twin_check(
      selDVs = c(pheno, prss),
      sep = sep, dzData = dzData, mzData = mzData, enforceSep = TRUE,
      nSib = 2, optimizer = optimizer
    )
  }

  if (!missing(mzData)) {mzData = xmu_make_mxData(mzData, manifests = vnames)}
  if (!missing(dzData)) {dzData = xmu_make_mxData(dzData, manifests = vnames)}

  x = y = 1

  if (length(prss) == 1) {

    if (is.null(name)) { name = "MRDoC"}

    top = mxModel("top",
      umxMatrix("BE", type = "Full",nrow=3,  ncol = 3, byrow = TRUE, 
                labels = c(NA, "g2", "b1", 
                           "g1", NA, "b2", 
                           NA, NA, NA),
                free = c(FALSE, FALSE, TRUE,
                         TRUE, FALSE, TRUE,
                         FALSE, FALSE, FALSE)),
      umxMatrix('I', type='Iden', nrow= 3,ncol= 3),
      umxMatrix('F', type='Full', nrow=5, ncol=6, free=FALSE, byrow = TRUE, 
                values=c(1,0,0,0,0,0,
                        0,1,0,0,0,0,
                        0,0,1,0,0,0,
                        0,0,0,1,0,0,
                        0,0,0,0,1,0)),
      umxMatrix('LY', type='Full',nrow=3, ncol = 3, free = FALSE, values = diag(3), labels = NA),
      umxMatrix('A', type='Symm', nrow=3,ncol = 3,byrow = TRUE, 
                labels=c("ab2","abraas",NA,
                         "abraas","as2",NA,
                         NA,NA,"x2"),
                free=c(TRUE,TRUE,FALSE,
                       TRUE,TRUE,FALSE,
                       FALSE,FALSE,TRUE)),
      umxMatrix('C', type='Symm',nrow=3, ncol = 3,byrow = TRUE, 
                labels =c("cb2","cbrccs",NA,
                         "cbrccs","cs2",NA,
                         NA,NA,NA),
                free=c(TRUE,TRUE,FALSE,
                       TRUE,TRUE,FALSE,
                       FALSE,FALSE,FALSE)),
      umxMatrix('E', type='Symm', nrow=3, ncol = 3,byrow = TRUE, 
                labels =c("eb2",NA,NA,
                         NA,"es2",NA,
                         NA,NA,NA),
                free= c(TRUE,FALSE,FALSE,
                        FALSE,TRUE,FALSE,
                        FALSE,FALSE,FALSE)),
      umxMatrix('dzmu', type='Full', nrow=1, ncol=6, free=TRUE, values= 0, byrow = TRUE, 
	  	labels=c('muPh1','muPh2','muPS1','muPh1','muPh2','muPS1')
	),
      mxAlgebra('mzmu', expression = dzmu%*%t(F)),
      mxAlgebra('A_'  , expression = solve(I - BE)%&%A),
      mxAlgebra('C_'  , expression = solve(I - BE)%&%C),
      mxAlgebra('E_'  , expression = solve(I - BE)%&%E),
      mxAlgebra('SPh' , expression = A_ + C_ + E_),
      mxAlgebra('Smz_', expression = rbind(
                           cbind(SPh, A_+C_),
                           cbind(A_+C_, SPh))),
		mxAlgebra('Sdz', expression=rbind(
                           cbind(SPh, .5%x%A_+C_),
                           cbind(.5%x%A_+C_, SPh))),
		mxAlgebra('Smz', expression= F%&%Smz_)
	  )

    MZ = mxModel("MZ",
      mxExpectationNormal(covariance = "top.Smz",means = "top.mzmu", vnames[1:5]),
      mxFitFunctionML()
    )

    DZ = mxModel("DZ",
         mxExpectationNormal(covariance = "top.Sdz",means = "top.dzmu", vnames),
         mxFitFunctionML()
    )

    if (sketch == F){
      MZ = mxModel(MZ, mzData)
      DZ = mxModel(DZ, dzData)
    }

    model = mxModel(name, top, MZ, DZ, mxFitFunctionMultigroup(c("MZ","DZ") ))

  } else if (length(prss) == 2) {

    if (is.null(name)) { name = "MRDoC2"}

    top = mxModel("top",
      umxMatrix("BE", type = "Full",nrow=4,  ncol = 4,byrow = TRUE, 
                labels = c(NA, "g2", "b1", "b4",
                           "g1", NA, "b2", "b3",
                           NA, NA, NA, NA,
                           NA, NA, NA, NA),
                free = c(FALSE, TRUE, TRUE, FALSE,
                         TRUE, FALSE, FALSE, TRUE,
                         FALSE, FALSE, FALSE, FALSE,
                         FALSE, FALSE, FALSE, FALSE)),
      umxMatrix('I', type='Iden', nrow= 4,ncol= 4 ),
      umxMatrix('F', type='Full', nrow=6, ncol=8, free=FALSE,byrow = TRUE, 
                values=c(1,0,0,0,0,0,0,0,
                         0,1,0,0,0,0,0,0,
                         0,0,1,0,0,0,0,0,
                         0,0,0,1,0,0,0,0,
                         0,0,0,0,1,0,0,0,
                         0,0,0,0,0,1,0,0)),
      umxMatrix('LY', type='Full',nrow=4, ncol = 4, free = FALSE, values = diag(4), labels = NA),
      umxMatrix('A', type='Symm', nrow=4,ncol = 4,byrow = TRUE, 
                labels=c("ab2","abraas",NA,NA,
                         "abraas","as2",NA,NA,
                         NA,NA,"x2"  ,"xrfy", 
                         NA,NA,"xrfy","y2"),
                free=c(TRUE,TRUE,FALSE,FALSE,
                       TRUE,TRUE,FALSE,FALSE,
                       FALSE,FALSE,TRUE,TRUE,
                       FALSE,FALSE,TRUE,TRUE)),
      umxMatrix('C', type='Symm',nrow=4, ncol = 4,byrow = TRUE, 
                labels =c("cb2","cbrccs",NA,NA,
                         "cbrccs","cs2" ,NA,NA,
                         NA,NA,NA,NA,
                         NA,NA,NA,NA),
                free=c(TRUE,TRUE,FALSE,FALSE,
                       TRUE,TRUE,FALSE,FALSE,
                       FALSE,FALSE,FALSE,FALSE,
                       FALSE,FALSE,FALSE,FALSE)),
      umxMatrix('E', type='Symm', nrow=4, ncol = 4,byrow = TRUE, 
                labels =c("eb2","ebrees",NA,NA,
                         "ebrees"," es2" ,NA,NA,
                         NA,NA,NA,NA,
                         NA,NA,NA,NA),
                free= c(TRUE,TRUE,FALSE,FALSE,
                        TRUE,TRUE,FALSE,FALSE,
                        FALSE,FALSE,FALSE,FALSE,
                        FALSE,FALSE,FALSE,FALSE)),
      umxMatrix('dzmu', type='Full', nrow=1, ncol=8, free=TRUE, byrow = TRUE, 
	  	values = 0, labels=c('muPh1','muPh2','muPS1','muPS2','muPh1','muPh2','muPS1','muPS2')
	),
      mxAlgebra('mzmu', expression = dzmu %*% t(F)),
      mxAlgebra('A_'  , expression = solve(I - BE) %&% A),
      mxAlgebra('C_'  , expression = solve(I - BE) %&% C),
      mxAlgebra('E_'  , expression = solve(I - BE) %&% E),
      mxAlgebra('SPh' , expression= A_ + C_ + E_),
      mxAlgebra('Smz_', expression=rbind(
                           cbind(SPh,A_+C_),
                           cbind(A_+C_,SPh))),
      mxAlgebra('Sdz', expression=rbind(
                           cbind(SPh,.5%x%A_+C_),
                           cbind(.5%x%A_+C_,SPh))),
      mxAlgebra('Smz', expression= F%&%Smz_)
	  )

    MZ = mxModel("MZ", mzData,
      mxExpectationNormal(covariance = "top.Smz",means = "top.mzmu", vnames[1:6]),
      mxFitFunctionML()
    )

    DZ = mxModel("DZ", dzData,
      mxExpectationNormal(covariance = "top.Sdz",means = "top.dzmu", vnames),
      mxFitFunctionML()
    )

    if (sketch == F){
      MZ = mxModel(MZ, mzData)
      DZ = mxModel(DZ, dzData)
    }

    model = mxModel(name, top, MZ, DZ, mxFitFunctionMultigroup(c("MZ","DZ") ))
  } else {
    stop("Only 1 or 2 PRSs are supported")
  }

  model = as(model, "MxModelMRDoC") # set class so that S3s dispatch e.g. plot()
  if (sketch == F){
    model = mxAutoStart(model)
    model  = xmu_safe_run_summary(model, autoRun = TRUE,  summary = summary, tryHard =  tryHard)
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
