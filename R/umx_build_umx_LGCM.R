#' Biometric latent growth curve on age, with an invariant covariate, for twins and siblings
#'
#' @description
#' Fits a biometric (twin) latent growth curve to repeated measures, with each person's age at
#' each wave as a definition variable, so growth is estimated in continuous age rather than at
#' fixed occasions. The growth factors (intercept, shape, and optionally a quadratic) are decomposed
#' into A, C, and E (and optionally a born-together variance T). An optional time-invariant covariate
#' (e.g. a biomarker or PRS) predicts the growth factors. The shape follows a chosen `link`:
#' linear, exponential, logistic, or Gompertz.
#'
#' @details
#' For person \eqn{t} at wave \eqn{w}, with age \eqn{Age_{tw}}:
#'
#' \deqn{y_{tw} = Int_t + Shape_t f(Age_{tw}) [+ Quad_t Age_{tw}^2] + e_{tw}}
#'
#' **Growth links**: only the shape loading \eqn{f()} changes with `link`. `rate` (r) and `mid` (m) are
#' shared parameters, free only when the link uses them.
#'
#' | link          | f(Age)                         | free shape parameters |
#' |:--------------|:-------------------------------|:----------------------|
#' | `linear`      | Age                            | none                  |
#' | `exponential` | 1 - exp(-r Age)                | r                     |
#' | `logistic`    | 1 / (1 + exp(-r (Age - m)))    | r, m                  |
#' | `gompertz`    | exp(-exp(-r (Age - m)))        | r, m                  |
#'
#' `quadratic = TRUE` adds an Age^2 growth factor to any link (with "linear", the classic
#' intercept + linear + quadratic LGCM).
#'
#' **Biometric decomposition**: A, C, and E (and T) are symmetric (co)variances over the growth factors
#' and the covariate, estimated directly (labels `A_2_1` etc.), or with `Chol = TRUE` as Cholesky
#' factors `al`, `cl`, `el` (`tl`), labels `a_2_1` etc. Both give the same fit when the solution is positive-definite. Measurement error is a residual on each wave, equated across
#' waves by default. Cross-person covariances are: MZ twins A + C (+ T); DZ twins `dzAr` A + `dzCr` C (+ T);
#' siblings `dzAr` A + `dzCr` C. With `dzCr = .25`, C becomes D (ADE).
#'
#' **Invariant covariate**: measured once per person without residual, kept orthogonal to the growth
#' factors in A/C/E/T, and linked to them only by regressions B (labels `b_Int_on_Z1` etc.), applied via
#' \eqn{(I - B)^{-1}}. With `invariant = NULL` this collapses to the plain growth model.
#'
#' **Born-together variance T**: shared in full by co-born pairs (MZ and DZ twins) but not by
#' siblings, so it is confounded with C in a twin-only design. `Tvar = TRUE` estimates T only when
#' siblings are present (a `_T3` member, or `sibData`), otherwise T is dropped with a message.
#'
#' **Trios**: if the data contain the third member's columns (e.g. `memory_V1_T3`, `AGE_V1_T3`), each
#' family is modeled as twin 1, twin 2, and a non-twin sibling (member 3), whose covariance with
#' each twin is the sibling covariance. Families without a sibling just have member 3 missing.
#'
#' **Missing ages**: ages are definition variables, so can't be missing. Missing ages are handled by
#' [xmu_update_covar()] wave by wave: the age is set to a placeholder and that wave's score to NA
#' (with a message if any observed scores are lost). Absent family members (e.g. no sibling) are fine.
#'
#' **Data**: give `mzData` and `dzData` (and optionally `sibData`) in wide format (one row per family),
#' or give `data` with a `zyg` column, in which case `mzData`, `dzData`, and `sibData` are the levels
#' of `zyg` to use (by default "MZ", "DZ", and "SIB", in any case). Data can also be piped in as the
#' first argument; data grouped by zygosity (e.g. `dplyr::group_by(zygosity)`) uses its grouping
#' column as `zyg`. No dplyr or tibble functions are used.
#'
#' **Fit**: because ages are definition variables, saturated reference models are not meaningful, so
#' the fit report shows -2LL, df, and AIC. Compare nested models with [umxCompare()], e.g. via
#' [umxModify()] with `regex = "^C_"` to drop C, or `"^T_"` to drop T (with `Chol = TRUE`: `"^c_"`, `"^t_"`).
#'
#' **Tips**: center age and scale it to decades so that `rate` starts near its scale. Sigmoid links need
#' enough waves to place `mid`. With `Chol = FALSE`, A/C/E are not constrained positive-definite: if the
#' optimizer finds an inadmissible solution, try other start values, or `Chol = TRUE`.
#'
#' @param name The name of the model (default "LGCM").
#' @param var Base names of the repeated measure, one per wave, without the twin suffix (e.g. `paste0("memory_V", 1:4)`).
#' @param age Base names of the age at each wave, in the same order as `var`. Used as definition variables.
#' @param invariant Optional base name(s) of time-invariant covariates (e.g. a PRS) predicting the growth factors (default NULL).
#' @param dzData DZ twin data (or, if `data` is given, the level(s) of `zyg` for DZ twins; default "DZ" in any case).
#' @param mzData MZ twin data (or, if `data` is given, the level(s) of `zyg` for MZ twins; default "MZ" in any case).
#' @param sibData Optional data on non-co-born sibling families (or a level of `zyg`; default "SIB" in any case, if present).
#' @param data Optional data with all families, split into groups by `zyg`. Can be piped in.
#' @param zyg If `data` is given, the column identifying zygosity (default "zygosity").
#' @param sep The separator between base names and the family-member number (default "_T": `var_T1`, `var_T2`, `var_T3`).
#' @param link Growth shape of the second factor: "linear" (default), "exponential", "logistic", or "gompertz".
#' @param quadratic Whether to add an Age^2 growth factor (default FALSE).
#' @param dzAr The DZ genetic correlation (default .5).
#' @param dzCr The DZ "C" correlation (default 1: set to .25 for an ADE model).
#' @param Tvar Whether to model a born-together variance T (default FALSE). Estimated only when siblings are present.
#' @param centerInvariant Whether to fix the covariate means at 0 (default TRUE), else estimate them.
#' @param equateResiduals Whether to use one residual variance across waves (default TRUE).
#' @param Chol Whether to fit A, C, E (and T) as Cholesky factors (e.g. `Apre = al %*% t(al)`), which keeps them positive-definite (default FALSE: estimate the symmetric matrices directly).
#' @param autoRun Whether to run the model (default), or just to create it and return without running.
#' @param tryHard Default ("yes") uses mxTryHard, "no" uses normal mxRun. Other options: "ordinal", "search".
#' @return - [OpenMx::mxModel()] with groups "MZ", "DZ" (and "SIB"), and shared parameters in "top":
#' `Atil`, `Ctil`, `Etil` (`Ttil`): latent A/C/E (T); `stdA`, `stdC`, `stdE` (`stdT`): their standardized
#' diagonals; `B`: covariate effects on the growth factors; `MeanLat`: growth factor means; `rate`, `mid`: shape parameters.
#' @export
#' @family Twin Modeling Functions
#' @seealso - [umxACE()], [umxSimplex()], [umxModify()], [umxCompare()]
#' @examples
#' \dontrun{
#' # Episodic memory at 4 waves, age centered and in decades, Gompertz shape, PRS predicting growth
#' m1 = umx_LGCM(var = paste0("memory_V", 1:4), age = paste0("AGE_V", 1:4, "_c10"),
#' 	invariant = "PRS_AD", dzData = dzData, mzData = mzData, link = "gompertz")
#' mxEval(top.B, m1)    # PRS effects on Int and Shape
#' mxEval(top.stdA, m1) # standardized A of the growth factors
#'
#' # Drop C to test AE
#' m2 = umxModify(m1, regex = "^C_", name = "AE", comparison = TRUE)
#'
#' # Twins and siblings in one data set with a zygosity column, piped in, with T
#' m3 = df |> umx_LGCM(var = paste0("memory_V", 1:4), age = paste0("AGE_V", 1:4, "_c10"),
#' 	zyg = "zygosity", Tvar = TRUE)
#'
#' # Or grouped by zygosity in a dplyr chain
#' m3 = df |> dplyr::group_by(zygosity) |>
#' 	umx_LGCM(var = paste0("memory_V", 1:4), age = paste0("AGE_V", 1:4, "_c10"), Tvar = TRUE)
#' }
umx_LGCM <- function(name = "LGCM", var, age, invariant = NULL, dzData = NULL, mzData = NULL, sibData = NULL, data = NULL, zyg = "zygosity", sep = "_T", link = c("linear", "exponential", "logistic", "gompertz"), quadratic = FALSE, dzAr = .5, dzCr = 1, Tvar = FALSE, centerInvariant = TRUE, equateResiduals = TRUE, Chol = FALSE, autoRun = getOption("umx_auto_run"), tryHard = c("yes", "no", "ordinal", "search")) {
	# =====================================================================
	# = Piped data (df |> umx_LGCM(var = , age = )) lands in the first =
	# = unnamed argument: take it as data and move other unnamed args back =
	# =====================================================================
	theCall  = as.list(sys.call())[-1]
	supplied = names(theCall)
	if(is.null(supplied)) supplied = rep("", length(theCall))
	argNames = names(formals(sys.function()))
	named    = argNames[stats::na.omit(pmatch(supplied[supplied != ""], argNames))]
	unnamed  = setdiff(argNames, named)[seq_len(sum(supplied == ""))]
	if(!("data" %in% named) && length(unnamed) && unnamed[1] != "data") {
		first = get(unnamed[1])
		if(is.list(first) && (unnamed[1] %in% c("name", "var", "age", "invariant") || !is.null(attr(first, "groups")))) {
			newArgs = c(mget(named), stats::setNames(mget(unnamed[-1]), unnamed[-length(unnamed)]))
			newArgs[["data"]] = first
			return(do.call(sys.function(), newArgs))
		}
	}

	link    = match.arg(link)
	tryHard = match.arg(tryHard)
	umx_check(length(var) == length(age), "stop", "var and age must have one entry per wave: you gave ", length(var), " var and ", length(age), " age")
	umx_check(length(var) >= 2, "stop", "Need at least 2 waves")
	if(dzCr == .25 && name == "LGCM") name = "LGCM_ADE"

	# Plain data.frame, without calling tibble or dplyr: lazy tables are realized by their as.data.frame method
	xmu_plain_df = function(d) {
		if(is.null(d) || is.character(d)) return(d)
		if(!is.data.frame(d)) d = as.data.frame(d)
		attr(d, "groups") = NULL
		class(d) = "data.frame"
		d
	}

	# =======================================================
	# = Split data into MZ, DZ, (SIB) by zyg or by grouping =
	# =======================================================
	if(!is.null(data)) {
		if(!is.data.frame(data)) data = as.data.frame(data)
		groupKeys = setdiff(names(attr(data, "groups")), ".rows")
		if(length(groupKeys) > 1) stop("Data are grouped by ", omxQuotes(groupKeys), ": group by zygosity only")
		if(length(groupKeys) == 1) {
			zyg = groupKeys
		} else if(!(zyg %in% names(data)) && (".group" %in% names(data))) {
			zyg = ".group"
		}
		data = xmu_plain_df(data)
		umx_check_names(zyg, data = data, die = TRUE)
		levs = unique(as.character(data[[zyg]]))
		mzLev  = if(is.null(mzData))  levs[toupper(levs) == "MZ"]  else mzData
		dzLev  = if(is.null(dzData))  levs[toupper(levs) == "DZ"]  else dzData
		sibLev = if(is.null(sibData)) levs[toupper(levs) == "SIB"] else sibData
		umx_check(length(mzLev) && length(dzLev), "stop", "Couldn't find MZ and DZ levels in ", omxQuotes(zyg), " (found ", omxQuotes(levs), "): set mzData and dzData to the levels to use")
		message("umx_LGCM: groups from ", omxQuotes(zyg), ": MZ = ", omxQuotes(mzLev), ", DZ = ", omxQuotes(dzLev), if(length(sibLev)) paste0(", SIB = ", omxQuotes(sibLev)))
		mzData  = data[data[[zyg]] %in% mzLev, , drop = FALSE]
		dzData  = data[data[[zyg]] %in% dzLev, , drop = FALSE]
		sibData = if(length(sibLev)) data[data[[zyg]] %in% sibLev, , drop = FALSE] else NULL
	}
	mzData  = xmu_plain_df(mzData)
	dzData  = xmu_plain_df(dzData)
	sibData = xmu_plain_df(sibData)
	umx_check(is.data.frame(mzData) && is.data.frame(dzData), "stop", "Need mzData and dzData (or data with a zyg column)")

	# ===================================================
	# = Dimensions, names, and trios (member 3 = a sib) =
	# ===================================================
	nv   = length(var)                 # waves
	nz   = length(invariant)           # time-invariant covariates
	ngf  = 2L + as.integer(quadratic)  # growth factors: Int, Shape, (Quad)
	Nlat = ngf + nz                    # latent set: growth factors + covariates
	nObs = nv + nz                     # observed variables per person

	memberVars = function(s) c(paste0(var, sep, s), if(nz > 0) paste0(invariant, sep, s))
	memberAges = function(s) paste0(age, sep, s)
	hasMember3 = function(d) is.null(d) || all(c(memberVars(3), memberAges(3)) %in% names(d))
	nSib = if(hasMember3(mzData) && hasMember3(dzData) && hasMember3(sibData)) 3L else 2L
	selVars = unlist(lapply(seq_len(nSib), memberVars))
	ageVars = unlist(lapply(seq_len(nSib), memberAges))
	for(d in list(mzData, dzData, sibData)) {
		if(!is.null(d)) umx_check_names(c(selVars, ageVars), data = d, die = TRUE)
	}
	if(nSib == 3) message("umx_LGCM: found member 3 (", omxQuotes(memberVars(3)[1]), "...): modeling twin + sibling trios")

	# T is identified only against a non-co-born relative: a sibling in the trio, or a SIB group
	haveT = isTRUE(Tvar) && (nSib == 3 || !is.null(sibData))
	if(isTRUE(Tvar) && !haveT) {
		message("umx_LGCM: Tvar = TRUE, but with no siblings the born-together variance T is confounded with C, so T will NOT be estimated. Add sibling data (members ", sep, "3, or sibData) to estimate T.")
	}

	# Age is a definition variable, so it can't be NA: xmu_update_covar sets a missing age to 99999 and that
	# wave's score to NA (each age affects only its own wave's score, so it is called wave by wave)
	xmu_fill_ages = function(d) {
		if(is.null(d)) return(NULL)
		d = d[, unique(c(selVars, ageVars)), drop = FALSE]
		yVars = unlist(lapply(seq_len(nSib), function(s) paste0(var, sep, s)))
		nObserved = sum(!is.na(d[, yVars]))
		for(w in seq_len(nv)) {
			d = xmu_update_covar(d, covar = age[w], pheno = var[w], sep = sep, nSib = nSib)
		}
		nLost = nObserved - sum(!is.na(d[, yVars]))
		if(nLost > 0) message("umx_LGCM: ", nLost, " observed scores had a missing age, and were set to NA")
		d
	}
	mzData  = xmu_fill_ages(mzData)
	dzData  = xmu_fill_ages(dzData)
	sibData = xmu_fill_ages(sibData)

	gfLabs = c("Int", "Shape", if(quadratic) "Quad")
	zLabs  = if(nz > 0) paste0("Z", seq_len(nz)) else character(0)

	# ==============================================================
	# = Latent (co)variance pattern: covariates orthogonal to growth =
	# ==============================================================
	symmLabels = function(prefix) {
		L = outer(seq_len(Nlat), seq_len(Nlat), function(i, j) paste0(prefix, "_", pmax(i, j), "_", pmin(i, j)))
		L[!symmFree] = NA
		L
	}
	symmFree = matrix(TRUE, Nlat, Nlat)
	startA   = matrix(.05, Nlat, Nlat); diag(startA) = .30
	if(nz > 0) {
		symmFree[(ngf + 1):Nlat, 1:ngf] = symmFree[1:ngf, (ngf + 1):Nlat] = FALSE
		startA[(ngf + 1):Nlat, 1:ngf]   = startA[1:ngf, (ngf + 1):Nlat]   = 0
	}

	# Covariate -> growth factor regressions
	Bfree = matrix(FALSE, Nlat, Nlat)
	Blab  = matrix(NA_character_, Nlat, Nlat)
	if(nz > 0) {
		Bfree[1:ngf, (ngf + 1):Nlat] = TRUE
		Blab[1:ngf, (ngf + 1):Nlat]  = outer(gfLabs, zLabs, function(g, z) paste0("b_", g, "_on_", z))
	}

	# Latent means (intercept starts at the first-wave mean) and residuals (covariates have none)
	meanVal = rep(0, Nlat); meanVal[1] = mean(mzData[[paste0(var[1], sep, 1)]], na.rm = TRUE)
	meanLab = paste0("mu_", c(gfLabs, zLabs))
	esLab   = c(if(equateResiduals) rep("es_y", nv) else paste0("es_y", 1:nv), if(nz > 0) paste0("es_z", 1:nz))

	# =====================================================================
	# = top: shared A/C/E(/T), B transform, means, residuals, and std     =
	# =====================================================================
	if(Chol) {
		# Cholesky: lower-triangular factors, so A = al %*% t(al) etc. are positive-definite
		lowerFree   = symmFree & lower.tri(symmFree, diag = TRUE)
		lowerLabels = function(prefix) {
			L = outer(seq_len(Nlat), seq_len(Nlat), function(i, j) paste0(prefix, "_", i, "_", j))
			L[!lowerFree] = NA
			L
		}
		startL = t(chol(startA))
		al   = mxMatrix("Lower", Nlat, Nlat, free = lowerFree, values = startL,           labels = lowerLabels("a"), name = "al")
		cl   = mxMatrix("Lower", Nlat, Nlat, free = lowerFree, values = startL * sqrt(.2), labels = lowerLabels("c"), name = "cl")
		el   = mxMatrix("Lower", Nlat, Nlat, free = lowerFree, values = startL,           labels = lowerLabels("e"), name = "el")
		tl   = mxMatrix("Lower", Nlat, Nlat, free = lowerFree, values = startL * sqrt(.2), labels = lowerLabels("t"), name = "tl")
		Apre = mxAlgebra(al %*% t(al), name = "Apre")
		Cpre = mxAlgebra(cl %*% t(cl), name = "Cpre")
		Epre = mxAlgebra(el %*% t(el), name = "Epre")
		Tpre = mxAlgebra(tl %*% t(tl), name = "Tpre")
		cholACE = list(al, cl, el)
		cholT   = list(tl)
	} else {
		# Direct-symmetric: estimate the (co)variances themselves
		Apre = mxMatrix("Symm", Nlat, Nlat, free = symmFree, values = startA,      labels = symmLabels("A"), name = "Apre")
		Cpre = mxMatrix("Symm", Nlat, Nlat, free = symmFree, values = startA * .2, labels = symmLabels("C"), name = "Cpre")
		Epre = mxMatrix("Symm", Nlat, Nlat, free = symmFree, values = startA,      labels = symmLabels("E"), name = "Epre")
		Tpre = mxMatrix("Symm", Nlat, Nlat, free = symmFree, values = startA * .2, labels = symmLabels("T"), name = "Tpre")
		cholACE = list()
		cholT   = list()
	}

	# Shape parameters, free only when the link uses them (unlabeled: referenced by name in the loadings)
	rate = mxMatrix("Full", 1, 1, free = (link != "linear"), values = .5, lbound = 1e-4, name = "rate")
	mid  = mxMatrix("Full", 1, 1, free = (link %in% c("logistic", "gompertz")), values = 0, name = "mid")

	B    = mxMatrix("Full", Nlat, Nlat, free = Bfree, values = ifelse(Bfree, .1, 0), labels = Blab, name = "B")
	I    = mxMatrix("Iden", Nlat, Nlat, name = "I")
	To   = mxAlgebra(solve(I - B), name = "To")
	Atil = mxAlgebra(To %*% Apre %*% t(To), name = "Atil")
	Ctil = mxAlgebra(To %*% Cpre %*% t(To), name = "Ctil")
	Etil = mxAlgebra(To %*% Epre %*% t(To), name = "Etil")
	Ttil = mxAlgebra(To %*% Tpre %*% t(To), name = "Ttil")

	dzArM = mxMatrix("Full", 1, 1, free = FALSE, values = dzAr, name = "dzAr")
	dzCrM = mxMatrix("Full", 1, 1, free = FALSE, values = dzCr, name = "dzCr")

	# Total latent variance, and latent covariance between relatives. T is shared by co-born twins only.
	if(haveT) {
		Vtil       = mxAlgebra(Atil + Ctil + Etil + Ttil, name = "Vtil")
		CovLat_MZ  = mxAlgebra(Atil + Ctil + Ttil, name = "CovLat_MZ")
		CovLat_DZ  = mxAlgebra(dzAr %x% Atil + dzCr %x% Ctil + Ttil, name = "CovLat_DZ")
	} else {
		Vtil       = mxAlgebra(Atil + Ctil + Etil, name = "Vtil")
		CovLat_MZ  = mxAlgebra(Atil + Ctil, name = "CovLat_MZ")
		CovLat_DZ  = mxAlgebra(dzAr %x% Atil + dzCr %x% Ctil, name = "CovLat_DZ")
	}
	CovLat_SIB = mxAlgebra(dzAr %x% Atil + dzCr %x% Ctil, name = "CovLat_SIB")

	MeanLat       = mxMatrix("Full", Nlat, 1, free = c(rep(TRUE, ngf), rep(!centerInvariant, nz)), values = meanVal, labels = meanLab, name = "MeanLat")
	MeanLat_tilde = mxAlgebra(To %*% MeanLat, name = "MeanLat_tilde")

	es_o  = mxMatrix("Diag", nObs, nObs, free = c(rep(TRUE, nv), rep(FALSE, nz)), values = c(rep(.3, nv), rep(0, nz)), labels = esLab, name = "es_o")
	VsObs = mxAlgebra(es_o %*% t(es_o), name = "VsObs")

	stdA = mxAlgebra(diag2vec(Atil / Vtil), name = "stdA")
	stdC = mxAlgebra(diag2vec(Ctil / Vtil), name = "stdC")
	stdE = mxAlgebra(diag2vec(Etil / Vtil), name = "stdE")
	stdT = mxAlgebra(diag2vec(Ttil / Vtil), name = "stdT")

	top = mxModel("top", cholACE, Apre, Cpre, Epre, rate, mid, B, I, To, Atil, Ctil, Etil, dzArM, dzCrM,
		Vtil, CovLat_MZ, CovLat_DZ, CovLat_SIB, MeanLat, MeanLat_tilde, es_o, VsObs, stdA, stdC, stdE)
	if(haveT) top = mxModel(top, cholT, Tpre, Ttil, stdT)

	# ===============================================================
	# = One family group: age loadings and expected moments         =
	# = rel: covariance of members 1 and 2 ("MZ", "DZ", or "SIB")   =
	# ===============================================================
	makeGroup = function(groupName, groupData, rel) {
		# Ages: one column per family member, one row per wave
		ages    = mxMatrix("Full", nv, nSib, free = FALSE, labels = matrix(paste0("data.", ageVars), nv, nSib), name = "ages")
		unit    = mxMatrix("Unit", nv, 1, name = "unit")
		unitAll = mxMatrix("Unit", nv, nSib, name = "unitAll")

		# The growth basis: one literal algebra for all members, chosen by link
		shape = switch(link,
			linear      = mxAlgebra(ages, name = "shape"),
			exponential = mxAlgebra(unitAll - exp(-(top.rate %x% ages)), name = "shape"),
			logistic    = mxAlgebra(unitAll / (unitAll + exp(-(top.rate %x% (ages - top.mid %x% unitAll)))), name = "shape"),
			gompertz    = mxAlgebra(exp(-exp(-(top.rate %x% (ages - top.mid %x% unitAll)))), name = "shape")
		)
		quad = mxAlgebra(ages * ages, name = "quad")

		# Growth loadings for each member (columns: Int, Shape, (Quad))
		if(quadratic) {
			f1 = mxAlgebra(cbind(unit, shape[, 1], quad[, 1]), name = "f1")
			f2 = mxAlgebra(cbind(unit, shape[, 2], quad[, 2]), name = "f2")
			f3 = mxAlgebra(cbind(unit, shape[, 3], quad[, 3]), name = "f3")
		} else {
			f1 = mxAlgebra(cbind(unit, shape[, 1]), name = "f1")
			f2 = mxAlgebra(cbind(unit, shape[, 2]), name = "f2")
			f3 = mxAlgebra(cbind(unit, shape[, 3]), name = "f3")
		}

		# Full loadings: growth loadings, plus an identity block for the covariates
		ZeroCols = mxMatrix("Full", nv, max(nz, 1), free = FALSE, values = 0, name = "ZeroCols")
		ZeroRows = mxMatrix("Full", max(nz, 1), ngf, free = FALSE, values = 0, name = "ZeroRows")
		I_Z      = mxMatrix("Iden", max(nz, 1), name = "I_Z")
		if(nz > 0) {
			L1 = mxAlgebra(rbind(cbind(f1, ZeroCols), cbind(ZeroRows, I_Z)), name = "L1")
			L2 = mxAlgebra(rbind(cbind(f2, ZeroCols), cbind(ZeroRows, I_Z)), name = "L2")
			L3 = mxAlgebra(rbind(cbind(f3, ZeroCols), cbind(ZeroRows, I_Z)), name = "L3")
		} else {
			L1 = mxAlgebra(f1, name = "L1")
			L2 = mxAlgebra(f2, name = "L2")
			L3 = mxAlgebra(f3, name = "L3")
		}

		CovTwin = switch(rel,
			MZ  = mxAlgebra(top.CovLat_MZ,  name = "CovTwin"),
			DZ  = mxAlgebra(top.CovLat_DZ,  name = "CovTwin"),
			SIB = mxAlgebra(top.CovLat_SIB, name = "CovTwin")
		)

		# Expected covariance blocks. Diagonals use top.Vtil, so T (if present) is in everyone's variance.
		S11 = mxAlgebra(L1 %*% top.Vtil %*% t(L1) + top.VsObs, name = "S11")
		S22 = mxAlgebra(L2 %*% top.Vtil %*% t(L2) + top.VsObs, name = "S22")
		S33 = mxAlgebra(L3 %*% top.Vtil %*% t(L3) + top.VsObs, name = "S33")
		S12 = mxAlgebra(L1 %*% CovTwin %*% t(L2), name = "S12")
		S13 = mxAlgebra(L1 %*% top.CovLat_SIB %*% t(L3), name = "S13")
		S23 = mxAlgebra(L2 %*% top.CovLat_SIB %*% t(L3), name = "S23")
		M1  = mxAlgebra(t(L1 %*% top.MeanLat_tilde), name = "M1")
		M2  = mxAlgebra(t(L2 %*% top.MeanLat_tilde), name = "M2")
		M3  = mxAlgebra(t(L3 %*% top.MeanLat_tilde), name = "M3")

		if(nSib == 3) {
			expCov = mxAlgebra(rbind(
				cbind(S11   , S12   , S13),
				cbind(t(S12), S22   , S23),
				cbind(t(S13), t(S23), S33)), dimnames = list(selVars, selVars), name = "expCov")
			expMean = mxAlgebra(cbind(M1, M2, M3), dimnames = list(NULL, selVars), name = "expMean")
			member3 = list(f3, L3, S33, S13, S23, M3)
		} else {
			expCov = mxAlgebra(rbind(
				cbind(S11   , S12),
				cbind(t(S12), S22)), dimnames = list(selVars, selVars), name = "expCov")
			expMean = mxAlgebra(cbind(M1, M2), dimnames = list(NULL, selVars), name = "expMean")
			member3 = list()
		}
		mxModel(groupName, ages, unit, unitAll, shape, f1, f2, L1, L2, CovTwin, S11, S22, S12, M1, M2, member3,
			if(quadratic) quad else list(), if(nz > 0) list(ZeroCols, ZeroRows, I_Z) else list(),
			expCov, expMean,
			mxExpectationNormal(covariance = "expCov", means = "expMean", dimnames = selVars),
			mxFitFunctionML(),
			mxData(groupData, type = "raw")
		)
	}

	# ============================================
	# = Assemble MZ, DZ, (SIB) and run if wanted =
	# ============================================
	MZ  = makeGroup("MZ", mzData, "MZ")
	DZ  = makeGroup("DZ", dzData, "DZ")
	if(is.null(sibData)) {
		model = mxModel(name, top, MZ, DZ, mxFitFunctionMultigroup(c("MZ", "DZ")))
	} else {
		SIB   = makeGroup("SIB", sibData, "SIB")
		model = mxModel(name, top, MZ, DZ, SIB, mxFitFunctionMultigroup(c("MZ", "DZ", "SIB")))
	}
	model = omxAssignFirstParameters(model)
	# Saturated models ignore the age definition variables, so report -2LL/AIC (refModels = FALSE)
	model = xmu_safe_run_summary(model, autoRun = autoRun, tryHard = tryHard, std = NULL, refModels = FALSE)
	return(model)
}
