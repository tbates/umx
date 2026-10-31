#' Biometric latent growth curve on age, for twins and siblings
#'
#' @description
#' Fits a biometric (twin) latent growth curve to repeated measures, with each person's age at
#' each wave as a definition variable, so growth is estimated in continuous age rather than at
#' fixed occasions. The growth factors (intercept, shape, and optionally a quadratic) decompose
#' into A, C, E (and optionally a born-together variance T). An optional time-invariant covariate
#' (e.g. a PRS) predicts the growth factors. The shape follows a chosen `link`:
#' linear, exponential, logistic, or Gompertz.
#'
#' @details For person `t` at wave `w`, with age \eqn{Age(t,w)}:
#' \deqn{y = Int + Shape f(Age) [+ Quad Age^2] + e}
#' Only the shape loading `f()` changes with `link`: linear `Age` (no extra parameters);
#' exponential `1 - exp(-r Age)` (free `rate` r); logistic `1 / (1 + exp(-r (Age - m)))`
#' (free `rate`, `mid` m); gompertz `exp(-exp(-r (Age - m)))` (free `rate`, `mid`).
#' `quadratic = TRUE` adds an `Age^2` growth factor to any link.
#' A, C, E (and T) are symmetric (co)variances over growth factors and covariates, estimated
#' directly (labels `A_2_1` etc.), or with `chol = TRUE` as Cholesky factors (`al`, `cl`, `el`,
#' `tl`; labels `a_2_1` etc.), which keeps them positive-definite. Residuals are equated
#' across waves by default. Cross-person covariances: MZ twins A + C (+ T); DZ twins
#' `dzAr` A + `dzCr` C (+ T); siblings `dzAr` A + `dzCr` C. With `dzCr = .25`, C becomes D (ADE).
#' An invariant covariate is orthogonal to growth in A/C/E/T and linked only by regressions
#' `B` (labels `b_Int_on_Z1`, etc.) via `(I - B)^-1`. T is shared in full by co-born pairs only,
#' so with `tVar = TRUE` it is estimated only when siblings are present (a `_T3` member or
#' `sibData`); otherwise it is dropped with a message. If `_T3` columns exist, member 3 is
#' modeled as a non-twin sibling. Missing ages are set to a placeholder wave by wave with
#' [xmu_update_covar()], blanking only that wave's score. Because ages are definition variables,
#' saturated reference models are not meaningful: fit is -2LL, df, AIC. `plot()` draws
#' the model-implied growth curve with observed wave means per group.
#'
#' @param name Model name (default "LGCM", or "LGCM_ADE" when `dzCr` is .25).
#' @param var Phenotype column stubs, one per wave (e.g. `c("y1", "y2", "y3", "y4")`).
#' @param age Age column stubs, one per wave, matching `var`.
#' @param invariant Optional time-invariant covariate stubs (e.g. a PRS), measured once per person.
#' @param dzData Wide DZ data (one row per family), or a zygosity level when `data` is given.
#' @param mzData Wide MZ data (one row per family), or a zygosity level when `data` is given.
#' @param sibData Optional wide sibling-pair data, or a zygosity level when `data` is given.
#' @param data Optional long data with a `zyg` column, split into mzData/dzData/sibData.
#' @param zyg Zygosity column in `data` (default "zygosity"), or the level to use.
#' @param sep Separator between stub and member number in wide column names (default "_T").
#' @param link Growth shape: "linear", "exponential", "logistic", or "gompertz".
#' @param quadratic Whether to add an Age^2 growth factor (default FALSE).
#' @param dzAr DZ/sibling A relatedness (default .5).
#' @param dzCr DZ/sibling C relatedness (default 1; .25 fits ADE).
#' @param tVar Whether to estimate born-together variance T (default FALSE; needs siblings).
#' @param centerInvariant Whether to center the invariant covariate mean (default TRUE).
#' @param equateResiduals Whether to equate wave residuals (default TRUE).
#' @param chol Whether to fit Cholesky factors instead of direct symmetric (co)variances (default FALSE).
#' @param autoRun Whether to run the model (default `getOption("umx_auto_run")`).
#' @param tryHard How hard to try fitting (default "yes"; nonlinear loadings are start-value sensitive). "auto" picks to suit the model.
#' @return Fitted [OpenMx::mxModel()] of subclass `MxModelLGCM`.
#' @export
#' @family Twin Modeling Functions
#' @seealso - [umxACE()], [umxSummary()], [umxModify()]
#' @examples
#' \dontrun{
#' set.seed(42)
#' mkFam = function(n, nSib) {
#' 	d = data.frame(row.names = seq_len(n))
#' 	for (s in seq_len(nSib)) {
#' 		base = rnorm(n, 0, .3)
#' 		for (w in 1:2) {
#' 			d[[paste0("age", w, "_T", s)]] = base + (w - 1) * .5 + rnorm(n, 0, .05)
#' 			d[[paste0("y", w, "_T", s)]] = 0
#' 		}
#' 	}
#' 	d
#' }
#' m0 = umxLGCM(var = c("y1", "y2"), age = c("age1", "age2"),
#' 		mzData = mkFam(50, 2), dzData = mkFam(50, 2), autoRun = FALSE)
#' umxSummary(m0)
#' }
umxLGCM <- function(name = "LGCM", var, age, invariant = NULL, dzData = NULL, mzData = NULL, sibData = NULL, 
	data = NULL, zyg = "zygosity", sep = "_T", link = c("linear", "exponential", "logistic", "gompertz"), 
	quadratic = FALSE, dzAr = .5, dzCr = 1, tVar = FALSE, centerInvariant = TRUE, 
	equateResiduals = TRUE, chol = FALSE, autoRun = getOption("umx_auto_run"), tryHard = c("yes", "no", "ordinal", "search", "auto")) {
	link    = match.arg(link)
	tryHard = match.arg(tryHard)
	umx_check(length(var) == length(age), "stop", "var and age must have one entry per wave: you gave ", length(var), " var and ", length(age), " age")
	umx_check(length(var) >= 2, "stop", "Need at least 2 waves")
	if(dzCr == .25 && name == "LGCM") name = "LGCM_ADE"

	# Plain data.frame, without calling tibble or dplyr: lazy tables are realized by their as.data.frame method
	plainDF = function(d) {
		if(is.null(d) || is.character(d)) return(d)
		if(!is.data.frame(d)) d = as.data.frame(d)
		attr(d, "groups") = NULL
		class(d) = "data.frame"
		d
	}

	# Split data into MZ, DZ, (SIB) by zyg or by grouping
	if(!is.null(data)) {
		if(!is.data.frame(data)) data = as.data.frame(data)
		groupKeys = setdiff(names(attr(data, "groups")), ".rows")
		if(length(groupKeys) > 1) stop("Data are grouped by ", omxQuotes(groupKeys), ": group by zygosity only")
		if(length(groupKeys) == 1) {
			zyg = groupKeys
		} else if(!(zyg %in% names(data)) && (".group" %in% names(data))) {
			zyg = ".group"
		}
		data = plainDF(data)
		umx_check_names(zyg, data = data, die = TRUE)
		levs = unique(as.character(data[[zyg]]))
		mzLev  = if(is.null(mzData))  levs[toupper(levs) == "MZ"]  else mzData
		dzLev  = if(is.null(dzData))  levs[toupper(levs) == "DZ"]  else dzData
		sibLev = if(is.null(sibData)) levs[toupper(levs) == "SIB"] else sibData
		umx_check(length(mzLev) && length(dzLev), "stop", "Couldn't find MZ and DZ levels in ", omxQuotes(zyg), " (found ", omxQuotes(levs), "): set mzData and dzData to the levels to use")
		message("umxLGCM: groups from ", omxQuotes(zyg), ": MZ = ", omxQuotes(mzLev), ", DZ = ", omxQuotes(dzLev), if(length(sibLev)) paste0(", SIB = ", omxQuotes(sibLev)))
		mzData  = data[data[[zyg]] %in% mzLev, , drop = FALSE]
		dzData  = data[data[[zyg]] %in% dzLev, , drop = FALSE]
		sibData = if(length(sibLev)) data[data[[zyg]] %in% sibLev, , drop = FALSE] else NULL
	}
	mzData  = plainDF(mzData)
	dzData  = plainDF(dzData)
	sibData = plainDF(sibData)
	umx_check(is.data.frame(mzData) && is.data.frame(dzData), "stop", "Need mzData and dzData (or data with a zyg column)")

	# Dimensions, names, and trios (member 3 = a sib)
	nv   = length(var)
	nz   = length(invariant)
	ngf  = 2L + as.integer(quadratic)
	Nlat = ngf + nz
	nObs = nv + nz

	memberVars = function(s) c(paste0(var, sep, s), if(nz > 0) paste0(invariant, sep, s))
	memberAges = function(s) paste0(age, sep, s)
	hasMember3 = function(d) is.null(d) || all(c(memberVars(3), memberAges(3)) %in% names(d))
	nSib = if(hasMember3(mzData) && hasMember3(dzData) && hasMember3(sibData)) 3L else 2L
	selVars = unlist(lapply(seq_len(nSib), memberVars))
	ageVars = unlist(lapply(seq_len(nSib), memberAges))
	for(d in list(mzData, dzData, sibData)) {
		if(!is.null(d)) umx_check_names(c(selVars, ageVars), data = d, die = TRUE)
	}
	if(nSib == 3) message("umxLGCM: found member 3 (", omxQuotes(memberVars(3)[1]), "...): modeling twin + sibling trios")

	# T is identified only against a non-co-born relative: a sibling in the trio, or a SIB group
	haveT = isTRUE(tVar) && (nSib == 3 || !is.null(sibData))
	if(isTRUE(tVar) && !haveT) {
		message("umxLGCM: tVar = TRUE, but with no siblings the born-together variance T is confounded with C, so T will NOT be estimated. Add sibling data (members ", sep, "3, or sibData) to estimate T.")
	}

	# Age is a definition variable, so it can't be NA: xmu_update_covar sets a missing age to 99999 and that
	# wave's score to NA (each age affects only its own wave's score, so it is called wave by wave)
	fillAges = function(d) {
		if(is.null(d)) return(NULL)
		d = d[, unique(c(selVars, ageVars)), drop = FALSE]
		yVars = unlist(lapply(seq_len(nSib), function(s) paste0(var, sep, s)))
		nObserved = sum(!is.na(d[, yVars]))
		for(w in seq_len(nv)) {
			d = xmu_update_covar(d, covar = age[w], pheno = var[w], sep = sep, nSib = nSib)
		}
		nLost = nObserved - sum(!is.na(d[, yVars]))
		if(nLost > 0) message("umxLGCM: ", nLost, " observed scores had a missing age, and were set to NA")
		d
	}
	mzData  = fillAges(mzData)
	dzData  = fillAges(dzData)
	sibData = fillAges(sibData)

	gfLabs = c("Int", "Shape", if(quadratic) "Quad")
	zLabs  = if(nz > 0) paste0("Z", seq_len(nz)) else character(0)

	# Latent (co)variance pattern: covariates orthogonal to growth
	symmFree = matrix(TRUE, Nlat, Nlat)
	startA   = matrix(.05, Nlat, Nlat); diag(startA) = .30
	if(nz > 0) {
		symmFree[(ngf + 1):Nlat, 1:ngf] = symmFree[1:ngf, (ngf + 1):Nlat] = FALSE
		startA[(ngf + 1):Nlat, 1:ngf]   = startA[1:ngf, (ngf + 1):Nlat]   = 0
	}
	symmLabels = function(prefix) {
		L = outer(seq_len(Nlat), seq_len(Nlat), function(i, j) paste0(prefix, "_", pmax(i, j), "_", pmin(i, j)))
		L[!symmFree] = NA
		L
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

	# top: shared A/C/E(/T), B transform, means, residuals, and std
	if(chol) {
		# Cholesky: lower-triangular factors, so A = al %*% t(al) etc. are positive-definite
		lowerFree   = symmFree & lower.tri(symmFree, diag = TRUE)
		lowerLabels = function(prefix) {
			L = outer(seq_len(Nlat), seq_len(Nlat), function(i, j) paste0(prefix, "_", i, "_", j))
			L[!lowerFree] = NA
			L
		}
		startL = t(base::chol(startA))
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

	MeanLat       = mxMatrix("Full", Nlat, 1, free = c(rep(TRUE, ngf), rep(!centerInvariant, nz)), labels = meanLab, values = meanVal, name = "MeanLat")
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

	# One family group: age loadings and expected moments. rel picks the twin covariance.
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

	# Assemble MZ, DZ, (SIB) and run if wanted
	MZ  = makeGroup("MZ", mzData, "MZ")
	DZ  = makeGroup("DZ", dzData, "DZ")
	if(is.null(sibData)) {
		model = mxModel(name, top, MZ, DZ, mxFitFunctionMultigroup(c("MZ", "DZ")))
	} else {
		SIB   = makeGroup("SIB", sibData, "SIB")
		model = mxModel(name, top, MZ, DZ, SIB, mxFitFunctionMultigroup(c("MZ", "DZ", "SIB")))
	}
	model = as(model, "MxModelLGCM") # set class so that S3s dispatch e.g. plot()
	model = omxAssignFirstParameters(model)
	# Saturated models ignore the age definition variables, so report -2LL/AIC (refModels = FALSE)
	model = xmu_safe_run_summary(model, autoRun = autoRun, tryHard = tryHard, std = NULL, refModels = FALSE)
	return(model)
}

#' Plot a biometric latent growth curve
#'
#' Trajectory figure for a fitted model built by [umxLGCM()]: the model-implied growth
#' curve over age, with observed wave means per zygosity group overlaid. Everything is
#' derived from the fitted model (age columns from the definition-variable labels,
#' phenotype/invariant split from the residual labels, growth link from the shape
#' formula), so no builder arguments are needed.
#'
#' @aliases plot.MxModelLGCM
#' @param x A fitted [umxLGCM()] model.
#' @param ageMin Smallest age drawn (default range of observed ages).
#' @param ageMax Largest age drawn (default range of observed ages).
#' @param n Points on the implied curve (default 100).
#' @param observed Whether to overlay observed wave means per group (default TRUE).
#' @param ... Ignored.
#' @return A ggplot graph object.
#' @export
#' @family Plotting functions
#' @seealso - [umxLGCM()], [umxPlotGxE()]
#' @examples
#' \dontrun{
#' set.seed(42)
#' mkFam = function(n, nSib) {
#' 	d = data.frame(row.names = seq_len(n))
#' 	for (s in seq_len(nSib)) {
#' 		base = rnorm(n, 0, .3)
#' 		for (w in 1:2) {
#' 			d[[paste0("age", w, "_T", s)]] = base + (w - 1) * .5 + rnorm(n, 0, .05)
#' 			d[[paste0("y", w, "_T", s)]] = 0
#' 		}
#' 	}
#' 	d
#' }
#' m0 = umxLGCM(var = c("y1", "y2"), age = c("age1", "age2"), mzData = mkFam(200, 2), dzData = mkFam(200, 2), autoRun = FALSE)
#' m0 = omxSetParameters(m0, labels = c("A_1_1", "mu_Int", "mu_Shape"), values = c(.5, 1, -.5))
#' sim = mxGenerateData(m0)
#' m1 = umxLGCM(var = c("y1", "y2"), age = c("age1", "age2"), mzData = sim$MZ, dzData = sim$DZ, tryHard = "no")
#' plot(m1)
#' }
umxPlotLGCM <- function(x = NA, ageMin = NULL, ageMax = NULL, n = 100, observed = TRUE, ...) {
	umx_check_model(x, "MxModelLGCM", beenRun = TRUE, callingFn = "umxPlotLGCM")
	model = x
	# Age columns, per member, from the definition-variable labels ("data.<col>")
	ageMat = sub("^data\\.", "", model$MZ$ages$labels)
	nSib = ncol(ageMat)
	nv = nrow(ageMat)
	# Phenotype/invariant split, from the residual labels. Phenotypes lead each member's
	# block: member s wave w is obsNames[(s - 1) * (nv + nz) + w]; ages follow all members.
	esDiag = diag(model$top$es_o$labels)
	nPheno = sum(grepl("^es_y", esDiag))
	phenoBlock = nPheno + sum(grepl("^es_z", esDiag))
	obsNames = colnames(model$MZ$data$observed)
	phenoCols = obsNames[seq_len(nPheno)]
	# Growth link, from our four known shape formulas
	shapeText = paste(gsub(" ", "", deparse(model$MZ$shape$formula)), collapse = "")
	link = if(shapeText == "ages") {
		"linear"
	} else if(grepl("exp(-exp(", shapeText, fixed = TRUE)) {
		"gompertz"
	} else if(grepl("/", shapeText, fixed = TRUE)) {
		"logistic"
	} else if(grepl("exp(", shapeText, fixed = TRUE)) {
		"exponential"
	} else {
		stop("umxPlotLGCM: unrecognized growth shape formula: ", shapeText)
	}
	# Fitted growth means at covariate means, via the model's own (I - B)^-1 path
	Nlat = nrow(model$top$B$values)
	meanTilde = drop(solve(diag(Nlat) - model$top$B$values) %*% model$top$MeanLat$values)
	quadratic = "mu_Quad" %in% names(omxGetParameters(model))
	rate = model$top$rate$values[1, 1]
	mid  = model$top$mid$values[1, 1]
	shapeFn = switch(link,
		linear      = function(a) a,
		exponential = function(a) 1 - exp(-rate * a),
		logistic    = function(a) 1 / (1 + exp(-rate * (a - mid))),
		gompertz    = function(a) exp(-exp(-rate * (a - mid)))
	)
	# Observed ages across groups and members, excluding the missing-age placeholder
	groups = setdiff(names(model@submodels), "top")
	allAges = unlist(lapply(groups, function(g) {
		d = model@submodels[[g]]$data$observed
		unlist(lapply(seq_len(nSib), function(s) d[, ageMat[, s]]))
	}))
	allAges = allAges[!is.na(allAges) & allAges < 90000]
	if(is.null(ageMin)) ageMin = min(allAges)
	if(is.null(ageMax)) ageMax = max(allAges)
	if(ageMin >= ageMax) stop("umxPlotLGCM: need ageMin < ageMax")
	ageGrid = seq(ageMin, ageMax, length.out = n)
	yHat = meanTilde[1] + meanTilde[2] * shapeFn(ageGrid) + if(quadratic) meanTilde[3] * ageGrid^2 else 0
	p = ggplot2::ggplot(data.frame(age = ageGrid, y = yHat), ggplot2::aes(x = age, y = y)) + ggplot2::geom_line()
	if(observed) {
		obs = do.call(rbind, lapply(groups, function(g) {
			d = model@submodels[[g]]$data$observed
			do.call(rbind, lapply(seq_len(nv), function(w) {
				a = unlist(lapply(seq_len(nSib), function(s) d[, ageMat[w, s]]))
				y = unlist(lapply(seq_len(nSib), function(s) d[, obsNames[(s - 1) * phenoBlock + w]]))
				a = a[!is.na(a) & a < 90000]
				data.frame(group = g, age = mean(a), y = mean(y, na.rm = TRUE))
			}))
		}))
		p = p + ggplot2::geom_point(data = obs, ggplot2::aes(x = age, y = y, color = group))
	}
	p + ggplot2::labs(x = "Age", y = sub("_T[0-9]+$", "", phenoCols[1]), title = model$name, color = "Group")
}

#' @export
plot.MxModelLGCM <- umxPlotLGCM

#' Summarize a biometric latent growth curve
#'
#' Table output for a model built by [umxLGCM()]: -2LL, degrees of freedom, and AIC,
#' plus the parameter estimates with standard errors. Saturated reference models are
#' not meaningful with age definition variables, so no absolute fit indices are computed.
#'
#' @aliases umxSummary.MxModelLGCM
#' @param model An [umxLGCM()] model that has been run.
#' @param digits Decimals for the printed output (default 2).
#' @param ... Ignored.
#' @return Invisibly, a list with the one-row fit table and the parameter table.
#' @export
#' @family Twin Modeling Functions
#' @seealso - [umxLGCM()], [umxSummary()]
umxSummaryLGCM <- function(model, digits = 2, ...) {
	umx_check_model(model, "MxModelLGCM", beenRun = TRUE, callingFn = "umxSummaryLGCM")
	fit = as.data.frame(suppressMessages(OpenMx::mxCompare(model)))
	fitRow = data.frame(model = model$name, minus2LL = fit$minus2LL, df = fit$df, AIC = fit$AIC, check.names = FALSE)
	print(fitRow, digits = digits, row.names = FALSE)
	tablePub = summary(model)$parameters[, c("name", "Estimate", "Std.Error")]
	umx_print(tablePub, digits = digits)
	invisible(list(fit = fitRow, parameters = tablePub))
}

#' @export
umxSummary.MxModelLGCM <- umxSummaryLGCM
