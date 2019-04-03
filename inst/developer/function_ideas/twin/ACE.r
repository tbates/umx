myACE = mxModel("ACE",
	mxModel("top",
		umxMatrix("meanG", "Full" , nrow = 1 , ncol = ntv, free = TRUE),
		umxMatrix("a"    , "Lower", nrow = nv, ncol = nv , free = TRUE),
		umxMatrix("c"    , "Lower", nrow = nv, ncol = nv , free = TRUE),
		umxMatrix("e"    , "Lower", nrow = nv, ncol = nv , free = TRUE),
		umxAlgebra("A", a %*% t(a)),
		umxAlgebra("C", c %*% t(c)),
		umxAlgebra("E", e %*% t(e)),
		umxAlgebra("ACE", A + C + E)
		umxAlgebra("AC" , A + C),
		umxAlgebra("hAC", 0.5 %x% A + C),
		umxAlgebra("expCovMZ", rbind(
			cbind(ACE, AC ),
		    cbind(AC , ACE))
		),
		umxAlgebra("expCovDZ", rbind(
			cbind(ACE, hAC),
		    cbind(hAC, ACE))
		)
	),
	mxModel("MZ", dataMZ, expMZ, funML
		mxExpectationNormal("top.expCovMZ", means = "top.meanG"), mxFitFunctionML(),
		mxData(mzData, type = "raw")
	),
	mxModel("DZ"dataDZ, expDZ, funML
		mxExpectationNormal("top.expCovDZ", means = "top.meanG"), mxFitFunctionML(),
		mxData(dzData, type = "raw")
	),
	mxFitFunctionMultigroup(c("MZ","DZ"))
)