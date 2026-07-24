# R/umx_build_umxCP.R

#' umxCP: Build and run a Common Pathway twin model
#'
#' @description
#' Make a 2-group Common Pathway twin model.
#' 
#' The common-pathway model  (aka "psychometric model" (McArdle and Goldsmith, 1990) provides a powerful tool
#' for theory-based testing of genetic and environmental differences. It proposes that `A`, `C`, and `E` components
#' act on a latent substrate (organ, mental mechanism etc.) and this is manifested in the measured phenotypes.
#'
#' `umxCP` supports this with pairs of mono-zygotic (MZ) and di-zygotic (DZ) twins reared together
#' to model the genetic and environmental structure of multiple phenotypes
#' (measured behaviors).
#' 
#' Common-pathway path diagram:
#' 
#' \if{html}{\figure{CP.svg}{options: style="width: 50\%;" alt="Figure: CP model"}}
#' \if{latex}{\figure{CP.pdf}{options: width=7cm}}
#' 
#' As can be seen, each phenotype also by default has A, C, and E influences specific to that phenotype.
#' 
#' Features include the ability to include more than one common pathway, to use ordinal data.
#' 
#' **note**: The function [umx_set_optimization_options()] allows users to see and set `mvnRelEps` and `mvnMaxPointsA`
#' mvnRelEps defaults to .005. For ordinal models, you might find that '0.01' works better.
#' 
#' @details
#' Like the [umxACE()] model, the CP model decomposes phenotypic variance
#' into additive genetic (A), unique environmental (E) and, optionally, either
#' common or shared-environment (C) or non-additive genetic effects (D).
#' 
#' Unlike the Cholesky, these factors do not act directly on the phenotype. Instead latent A, 
#' C, and E influences impact on one or more latent factors which in turn account for variance in the phenotypes (see Figure).
#' 
#' 
#' **Data Input**
#' Currently, the umxCP function accepts only raw data. This may change in future versions.
#' 
#' **Ordinal Data**
#' 
#' In an important capability, the model transparently handles ordinal (binary or multi-level
#' ordered factor data) inputs, and can handle mixtures of continuous, binary, and ordinal
#' data in any combination.
#' 
#' **Additional features**
#' 
#' The umxCP function supports varying the DZ genetic association (defaulting to .5)
#' to allow exploring assortative mating effects, as well as varying the DZ \dQuote{C} factor
#' from 1 (the default for modeling family-level effects shared 100% by twins in a pair),
#' to .25 to model dominance effects.
#'
#' **Matrices and Labels in CP model**
#' 
#' A good way to see which matrices are used in umxCP is to run an example model and plot it.
#'
#' All the shared matrices are in the model "top".
#' 
#' Matrices `top$as`, `top$cs`, and `top$es` contain the path loadings specific to each variable on their diagonals.
#' 
#' So, to see the 'as' values, labels, or free states, you can say:
#'
#' `m1$top$as$values`
#' 
#' `m1$top$as$free`
#' 
#' `m1$top$as$labels`
#' 
#' Labels relevant to modifying the specific loadings take the form "as_r1c1", "as_r2c2" etc.
#' 
#' The common-pathway loadings on the factors are in matrices `top$a_cp`, `top$c_cp`, `top$e_cp`.
#'
#' The common factors themselves are in the matrix `top$cp_loadings` (an nVar * 1 matrix)
#'	
#' Less commonly-modified matrices are the mean matrix `expMean`. This has 1 row, and the columns are laid out for each variable for twin 1, followed by each variable for twin 2.
#' So, in a model where the means for twin 1 and twin 2 had been equated (set = to T1), you could make them independent again with this line:
#'
#' `m1$top$expMean$labels[1,4:6] = c("expMean_r1c4", "expMean_r1c5", "expMean_r1c6")`
#' 
#' For a deep-dive, see [xmu_make_TwinSuperModel()]
#' 
#' @param name The name of the model (defaults to "CP").
#' @param selDVs The variables to include.
#' omit sep in selDVs, i.e., just "dep" not c("dep_T1", "dep_T2").
#' @param selCovs basenames for covariates
#' @param sep (required) The suffix for twin 1 and twin 2, often "_T".
#' @param dzData The DZ dataframe.
#' @param mzData The MZ dataframe.
#' @param nFac How many common factors (default = 1)
#' @param type One of "Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS"
#' @param allContinuousMethod "cumulants" or "marginals". Used in all-continuous WLS data to determine if a means model needed.
#' @param data If provided, dzData and mzData are treated as valid levels of zyg to select() data sets (default = NULL)
#' @param zyg If data provided, this column is used to select rows by zygosity (Default = "zygosity")
#' @param correlatedACE DON'T USE THIS! Allows correlations between the factors built by each of the a, c, and e matrices. Default = FALSE.
#' @param dzAr The DZ genetic correlation (defaults to .5, vary to examine assortative mating).
#' @param dzCr The DZ "C" correlation (defaults to 1: set to .25 to make an ADE model).
#' @param autoRun Whether to run the model (default), or just to create it and return without running.
#' @param tryHard Default ("yes") uses mxTryHard, "no" uses normal mxRun. Other options: "ordinal", "search"
#' @param optimizer optionally set the optimizer (default NULL does nothing).
#' @param weightVar If provided, a vector objective will be used to weight the data. (default = NULL).
#' @param bVector Whether to compute row-wise likelihoods (defaults to FALSE).
#' @param boundDiag = Numeric lbound for diagonal of the a_cp, c_cp, & e_cp matrices. Set = NULL to ignore.
#' @param addStd Whether to add the algebras to compute a std model (defaults to TRUE).
#' @param addCI Whether to add the interval requests for CIs (defaults to TRUE).
#' @param numObsDZ = not yet implemented: Ordinal Number of DZ twins: Set this if you input covariance data.
#' @param numObsMZ = not yet implemented: Ordinal Number of MZ twins: Set this if you input covariance data.
#' @param equateMeans Whether to equate the means across twins (defaults to TRUE).
#' @param freeLowerA (ignore): Whether to leave the lower triangle of A free (default = FALSE).
#' @param freeLowerC (ignore): Whether to leave the lower triangle of C free (default = FALSE).
#' @param freeLowerE (ignore): Whether to leave the lower triangle of E free (default = FALSE).
#' @param correlatedA deprecated.
#' @return - [OpenMx::mxModel()]
#' @export
#' @family Twin Modeling Functions
#' @seealso - [umxSummaryCP()], [umxPlotCP()]. See [umxRotate.MxModelCP()] to rotate the factor loadings of a [umxCP()] model. See [umxACE()] for more examples of twin modeling. 
#' [plot()] and [umxSummary()] work for all twin models, e.g., [umxIP()], [umxCP()], [umxGxE()], and [umxACE()].
#' @references * Martin, N. G., & Eaves, L. J. (1977). The Genetical Analysis of Covariance Structure. *Heredity*, **38**, 79-95.
#' * Kendler, K. S., Heath, A. C., Martin, N. G., & Eaves, L. J. (1987). Symptoms of anxiety and symptoms of depression. 
#' Same genes, different environments? *Archives of General Psychiatry*, **44**, 451-457. \doi{10.1001/archpsyc.1987.01800170073010}.
#' * McArdle, J. J., & Goldsmith, H. H. (1990). Alternative common factor models for multivariate biometric analyses.
#' *Behavior Genetics*, **20**, 569-608. \doi{10.1007/BF01065873}.
#' * <https://github.com/tbates/umx>
#'
#' @examples
#' \dontrun{
#' # ========================================================
#' # = Run a 3-factor Common pathway twin model of 6 traits =
#' # ========================================================
#' require(umx)
#' data(GFF)
#' mzData = subset(GFF, zyg_2grp == "MZ")
#' dzData = subset(GFF, zyg_2grp == "DZ")
#' #  # These will be expanded into "gff_T1" "gff_T2" etc.
#' selDVs = c("gff", "fc", "qol", "hap", "sat", "AD") 
#' m1 = umxCP(selDVs = selDVs, sep = "_T", nFac = 3, tryHard = "yes",
#' 		dzData = dzData, mzData = mzData)
#'
#' # Shortcut using "data ="
#' selDVs = c("gff", "fc", "qol", "hap", "sat", "AD") 
#' m1 = umxCP(selDVs= selDVs, nFac= 3, data=GFF, zyg="zyg_2grp")
#'
#' # ===================
#' # = Do it using WLS =
#' # ===================
#' m2 = umxCP("new", selDVs = selDVs, sep = "_T", nFac = 3, optimizer = "SLSQP",
#' 		dzData = dzData, mzData = mzData, tryHard = "ordinal", 
#'		type= "DWLS", allContinuousMethod='marginals'
#' )
#' 
#' # =================================================
#' # = Find and test dropping of shared environment  =
#' # =================================================
#' # Show all labels for C parameters 
#' umxParameters(m1, patt = "^c")
#' # Test dropping the 9 specific and common-factor C paths
#' m2 = umxModify(m1, regex = "(cs_.*$)|(c_cp_)", name = "dropC", comp = TRUE)
#' umxSummaryCP(m2, comparison = m1, file = NA)
#' umxCompare(m1, m2)
#' 
#' # =======================================
#' # = Mixed continuous and binary example =
#' # =======================================
#' data(GFF)
#' # Cut to form umxFactor 20% depressed  DEP
#' cutPoints = quantile(GFF[, "AD_T1"], probs = .2, na.rm = TRUE)
#' ADLevels  = c('normal', 'depressed')
#' GFF$DEP_T1 = cut(GFF$AD_T1, breaks = c(-Inf, cutPoints, Inf), labels = ADLevels) 
#' GFF$DEP_T2 = cut(GFF$AD_T2, breaks = c(-Inf, cutPoints, Inf), labels = ADLevels) 
#' ordDVs = c("DEP_T1", "DEP_T2")
#' GFF[, ordDVs] = umxFactor(GFF[, ordDVs])
#' 
#' # These will be expanded into "gff_T1" "gff_T2" etc.
#' selDVs = c("gff","fc","qol","hap","sat","DEP") 
#' mzData = subset(GFF, zyg_2grp == "MZ")
#' dzData = subset(GFF, zyg_2grp == "DZ")
#' 
#' # umx_set_optimizer("NPSOL")
#' # umx_set_optimization_options("mvnRelEps", .01)
#' m1 = umxCP(selDVs = selDVs, sep = "_T", nFac = 3, dzData = dzData, mzData = mzData)
#' m2 = umxModify(m1, regex = "(cs_r[3-5]|c_cp_r[12])", name = "dropC", comp= TRUE)
#' 
#' # Do it using WLS
#' m3 = umxCP(selDVs = selDVs, sep = "_T", nFac = 3, dzData = dzData, mzData = mzData,
#'			tryHard = "ordinal", type= "DWLS")
#'	# TODO umxCPL fix WLS here
#'	# label at row 1 and column 1 of matrix 'top.binLabels'' in model 'CP3fac' : object 'Vtot'
#'
#' # ==============================
#' # = Correlated factors example =
#' # ==============================
#' # ====================
#' # = DON'T USE THIS!!! =
#' # ====================
#' data(GFF)
#' mzData = subset(GFF, zyg_2grp == "MZ")
#' dzData = subset(GFF, zyg_2grp == "DZ")
#' selDVs = c("gff", "fc", "qol", "hap", "sat", "AD")
#' m1 = umxCP("base_model", selDVs = selDVs, sep = "_T", correlatedACE = TRUE, 
#' 	 dzData = dzData, mzData = mzData, nFac = 3, tryHard = "yes")
#' 
#' # What are the ace covariance labels? (two ways to get)
#' umx_lower.tri(m1$top$a_cp$labels)
#' parameters(m1, patt = "[ace]_cp")
#'
#' # 1. Now allow a1 and a2 to correlate
#' m2=umxModify(m1,regex="a_cp_r2c1",name="a2_a1_cov",free=TRUE,tryHard="yes")
#' umxCompare(m2, m1)
#'
#' # 2. Drop all (a|c|e) correlations from a model
#' tmp= namez(umx_lower.tri(m2$top$a_cp$labels), "a_cp", replace= "[ace]_cp")
#' m3 = umxModify(m2, regex= tmp, comparison = TRUE)
#' }
umxCP <- function(name = "CP", selDVs, selCovs=NULL, dzData= NULL, mzData= NULL, sep = NULL, nFac = 1, type = c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS"), data = NULL, zyg = "zygosity", allContinuousMethod = c("cumulants", "marginals"), correlatedACE = FALSE, dzAr= .5, dzCr= 1, autoRun = getOption("umx_auto_run"), tryHard = c("yes", "no", "ordinal", "search"), optimizer = NULL, equateMeans= TRUE, weightVar = NULL, bVector = FALSE, boundDiag = 0, addStd = TRUE, addCI = TRUE, numObsDZ = NULL, numObsMZ = NULL, freeLowerA = FALSE, freeLowerC = FALSE, freeLowerE = FALSE, correlatedA = "deprecated") {
	# TODO umxCP: Add covariates to means model: Will involve xmu_make_top_twin? also means model?
	tryHard             = match.arg(tryHard)
	type                = match.arg(type)
	allContinuousMethod = match.arg(allContinuousMethod)
	nSib                = 2 # Number of siblings in a twin pair.
	# Add nFac to base name if no user-set name provided.
	if(name == "CP"){ name = paste0(name, nFac, "fac") }

	# if data provided create twin files 
	if(!is.null(data)){
		if(is.null(sep)){ sep = "_T" }
		# avoid ingesting tibbles
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
	xmu_twin_check(selDVs= selDVs, dzData = dzData, mzData = mzData, enforceSep = TRUE, sep = sep, nSib = nSib, optimizer = optimizer)
	
	# New-style build-block: Expand var names if necessary and make the basic components of a twin model
	selVars   = xmu_twin_upgrade_selDvs2SelVars(selDVs = selDVs, sep = sep, nSib= nSib)
	nVar      = length(selVars)/nSib; # Number of dependent variables per **INDIVIDUAL** (so x2 per family)
	model     = xmu_make_TwinSuperModel(name=name, mzData = mzData, dzData = dzData, selDVs = selDVs, selCovs= selCovs, sep = sep, type = type, allContinuousMethod = allContinuousMethod, 	numObsMZ = numObsMZ, numObsDZ = numObsDZ, nSib= nSib, equateMeans = equateMeans, weightVar = weightVar, bVector = FALSE, verbose= FALSE)
	tmp       = xmu_starts(mzData, dzData, selVars = selDVs, sep = sep, nSib = nSib, varForm = "Cholesky", equateMeans= equateMeans, SD= TRUE, divideBy = 3)
	varStarts = tmp$varStarts
	if(correlatedA != "deprecated"){
		message("Polite message: As of February 2021, please use 'correlatedACE' in place of 'correlatedA'.
		The new behavior with 'correlatedACE' still makes a_cp_matrix etc. type Lower, but leaves the off-diagonal elements fixed at zero.
		(you can then free each one as you choose)")
		correlatedACE = TRUE
	}
	if(correlatedACE){
		umx_msg("Polite message: correlatedACE is in alpha: Results are not valid currently!!! Do not use!!!")
		if(correlatedA != "deprecated"){
			a_cp_matrix = umxMatrix("a_cp", "Lower", nFac, nFac, free = TRUE, values = 0) # Latent common factor
			c_cp_matrix = umxMatrix("c_cp", "Lower", nFac, nFac, free = TRUE, values = 0) # latent common factor Common environmental path coefficients
			e_cp_matrix = umxMatrix("e_cp", "Lower", nFac, nFac, free = TRUE, values = 0) # latent common factor Unique environmental path coefficients
		}else{
			a_cp_matrix = umxMatrix("a_cp", "Lower", nFac, nFac, free = FALSE, values = 0) # Latent common factor
			c_cp_matrix = umxMatrix("c_cp", "Lower", nFac, nFac, free = FALSE, values = 0) # latent common factor Common environmental path coefficients
			e_cp_matrix = umxMatrix("e_cp", "Lower", nFac, nFac, free = FALSE, values = 0) # latent common factor Unique environmental path coefficients			
			diag(a_cp_matrix$free) = TRUE
			diag(c_cp_matrix$free) = TRUE
			diag(e_cp_matrix$free) = TRUE
		}
		diag(a_cp_matrix$values) = .7
		diag(c_cp_matrix$values) = .0
		diag(e_cp_matrix$values) = .7

		a_cp_matrix$lbound[lower.tri(a_cp_matrix$lbound)] = -1
		c_cp_matrix$lbound[lower.tri(c_cp_matrix$lbound)] = -1
		e_cp_matrix$lbound[lower.tri(e_cp_matrix$lbound)] = -1
		a_cp_matrix$ubound[lower.tri(a_cp_matrix$ubound)] =  1
		c_cp_matrix$ubound[lower.tri(c_cp_matrix$ubound)] =  1
		e_cp_matrix$ubound[lower.tri(e_cp_matrix$ubound)] =  1
	} else {
		a_cp_matrix = umxMatrix("a_cp", "Diag" , nFac, nFac, free = TRUE, values = .7)
		c_cp_matrix = umxMatrix("c_cp", "Diag" , nFac, nFac, free = TRUE, values = .0)
		e_cp_matrix = umxMatrix("e_cp", "Diag" , nFac, nFac, free = TRUE, values = .7)
	}
	# Finish building top
	top = mxModel(model$top,
		umxMatrix("dzAr"        , "Full", 1, 1, free = FALSE, values = dzAr),
		umxMatrix("dzCr"        , "Full", 1, 1, free = FALSE, values = dzCr),
		umxMatrix("nFac_UnitCol", "Unit" , nrow = nFac, ncol = 1),
		umxMatrix("nFac_Iden"   , "Iden" , nrow = nFac, ncol = nFac),
		umxMatrix("nFac_Lower1s", "Lower", nrow = nFac, ncol = nFac, values= 1),
		# Latent common factor genetic paths
		a_cp_matrix, c_cp_matrix, e_cp_matrix,
		# Constrain variance of latent phenotype factor to 1.0
		# Multiply by each path coefficient by its inverse to get variance component
		mxAlgebra(name = "A_cp", a_cp %*% t(a_cp)  ), # A_cp variance
		mxAlgebra(name = "C_cp", c_cp %*% t(c_cp)  ), # C_cp variance
		mxAlgebra(name = "E_cp", e_cp %*% t(e_cp)  ), # E_cp variance
		mxAlgebra(name = "L"   , A_cp + C_cp + E_cp), # total common factor covariance (a+c+e)
		
		# multiply by lower 1s?
		# mxAlgebra(name = "sumL", nFac_Lower1s %*% L),
		# mxConstraint(name = "fix_CP_variances_to_1", sumL[nFac,1:nFac] == nFac_UnitCol),
		
		mxAlgebra(name = "diagL", diag2vec(L)),
		mxConstraint(name = "fix_CP_variances_to_1", diagL == nFac_UnitCol),

		umxMatrix("as", "Lower", nVar, nVar, free = TRUE, values = .5), # Additive gen path 
		umxMatrix("cs", "Lower", nVar, nVar, free = TRUE, values = .1), # Common env path 
		umxMatrix("es", "Lower", nVar, nVar, free = TRUE, values = .5), # Unique env path
		umxMatrix("cp_loadings", "Full", nVar, nFac, free = TRUE, values = .5), # loadings on latent phenotype

		# Quadratic multiplication to add cp_loading effects
		mxAlgebra(name = "A"  , cp_loadings %&% (A_cp * nFac_Iden) + as %*% t(as)), # Additive genetic variance
		mxAlgebra(name = "C"  , cp_loadings %&% (C_cp * nFac_Iden) + cs %*% t(cs)), # Common environmental variance
		mxAlgebra(name = "E"  , cp_loadings %&% (E_cp * nFac_Iden) + es %*% t(es)), # Unique environmental variance
		mxAlgebra(name = "ACE", A + C + E),
		mxAlgebra(name = "AC" , A + C),
		mxAlgebra(name = "hAC", (dzAr %x% A) + (dzCr %x% C)),
		mxAlgebra(name= "expCovMZ", dimnames = list(selVars, selVars), 
					rbind( cbind(ACE, AC), 
		                   cbind(AC , ACE))
		),
		mxAlgebra(name= "expCovDZ", dimnames = list(selVars, selVars), 
					rbind( cbind(ACE, hAC),
		                   cbind(hAC, ACE))
		)
	)
	model = mxModel(model, top) 

	if(!freeLowerA){
		toset  = model$top$matrices$as$labels[lower.tri(model$top$matrices$as$labels)]
		model = omxSetParameters(model, labels = toset, free = FALSE, values = 0)
	}
	if(!freeLowerC){
		toset  = model$top$matrices$cs$labels[lower.tri(model$top$matrices$cs$labels)]
		model = omxSetParameters(model, labels = toset, free = FALSE, values = 0)
	}
	if(!freeLowerE){
		toset  = model$top$matrices$es$labels[lower.tri(model$top$matrices$es$labels)]
		model = omxSetParameters(model, labels = toset, free = FALSE, values = 0)
	}
	if(addStd){
		newTop = mxModel(model$top,
			# nVar Identity matrix
			mxMatrix(name = "I", "Iden", nVar, nVar),
			# inverse of standard deviation diagonal  (same as "(\sqrt(I.Vtot))~"
			mxAlgebra(name = "SD", solve(sqrt(I * ACE))),
			# Standard specific path coefficients
			mxAlgebra(name = "as_std", SD %*% as), # standardized a
			mxAlgebra(name = "cs_std", SD %*% cs), # standardized c
			mxAlgebra(name = "es_std", SD %*% es), # standardized e
			# Standardize loadings on Common factors
			mxAlgebra(name = "cp_loadings_std", SD %*% cp_loadings) # Standardized path coefficients (general factor(s))
		)
		model = mxModel(model, newTop)
		if(addCI){
			# TODO umxCP: break these CIs out into single labels?
			model = mxModel(model, mxCI(c('top.a_cp', 'top.c_cp', 'top.e_cp', 'top.as_std', 'top.cs_std', 'top.es_std', 'top.cp_loadings_std')))
		}
	}
	if(!is.null(boundDiag)){
		if(!is.numeric(boundDiag)){
			stop("boundDiag must be a digit or vector of numbers. You gave me a ", class(boundDiag))
		} else {				
			if(length(boundDiag) > 1 ){
				if(length(boundDiag) != length(diag(newLbound)) ){
					stop("Typically boundDiag is 1 digit: if more, must be size of diag(a_cp)")
				}
			}
			newCPLbound = model$top$matrices$a_cp@lbound
			diag(newCPLbound) = boundDiag; 
			model$top$a_cp$lbound = newCPLbound
			model$top$c_cp$lbound = newCPLbound
			model$top$e_cp$lbound = newCPLbound
			newSpecLbound = model$top$matrices$as@lbound
			diag(newSpecLbound) = boundDiag; 
			model$top$as$lbound = newSpecLbound
			model$top$cs$lbound = newSpecLbound
			model$top$es$lbound = newSpecLbound
		}
	}
	# Set values with the same label to the same start value... means for instance.
	model = omxAssignFirstParameters(model)
	model = as(model, "MxModelCP")
	model = xmu_safe_run_summary(model, autoRun = autoRun, tryHard = tryHard)	
	return(model)
}
