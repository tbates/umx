myACE = mxModel("ACE",
	mxModel("top",
		mxMatrix(name = "meanG", "Full" , nrow = 1 , ncol = ntv, free = T),
		mxMatrix(name = "a"    , "Lower", nrow = nv, ncol = nv , free = T),
		mxMatrix(name = "c"    , "Lower", nrow = nv, ncol = nv , free = T),
		mxMatrix(name = "e"    , "Lower", nrow = nv, ncol = nv , free = T),
		mxAlgebra(name = "A", a %*% t(a)),
		mxAlgebra(name = "C", c %*% t(c)),
		mxAlgebra(name = "E", e %*% t(e)),
		mxAlgebra(name = "ACE", A + C + E)
		mxAlgebra(name = "AC" , A + C),
		mxAlgebra(name = "hAC", 0.5 %x% A + C),
		mxAlgebra(rbind(cbind(ACE, AC ),
		                cbind(AC , ACE)), name = "expCovMZ"),
		mxAlgebra(rbind(cbind(ACE, hAC),
		                cbind(hAC, ACE)), name = "expCovDZ")
	),
	mxModel("MZ", dataMZ, expMZ, funML
		mxExpectationNormal("top.expCovMZ", means = "top.meanG"),
		mxFitFunctionML(),
		mxData(mzData, type = "raw")
	),
	mxModel("DZ"dataDZ, expDZ, funML
		mxExpectationNormal("top.expCovDZ", means = "top.meanG"),
		mxFitFunctionML(),
		mxData(dzData, type = "raw")
	),
	mxFitFunctionMultigroup(c("MZ","DZ"))
)