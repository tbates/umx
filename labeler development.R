umxLabel <- function(obj, suffix = "", baseName = NA, setfree = F, drop = 0, jiggle = NA, boundDiag = NA, verbose = F) {	
	# Purpose: Label the cells of a matrix, OR the matrices of a RAM model
	# nb: obj must be either an mxModel or an mxMatrix
	# Use case:
	# m1 = umxLabel(m1, suffix = "")
	# umxLabel(mxMatrix("Full", 3,3, values = 1:9, name = "a"))
	if (is(obj, "MxMatrix") ) { 
		# label an mxMatrix
		xmuLabel_Matrix(obj, baseName, setfree, drop, jiggle, boundDiag, suffix)
	} else if (umxModelIsRAM(obj)) { 
		# label a RAM model
		if(verbose){message("RAM")}
		return(xmuLabel_RAM_Model(obj, suffix))
	} else if (is(obj, "MxModel")) {
		# label a non-RAM matrix model
		return(xmuLabel_MATRIX_Model(obj, suffix))
	} else {
		stop("'obj' must be an OpenMx RAM model OR an mxMatrix")
	}
}
# =============
# = unit test =
# =============

# ========================
# = 1.  test on matrix   =
# ========================
umxLabel(mxMatrix(name = "a", type = "Lower", nrow = 3, ncol = 3))@labels
umxLabel(mxMatrix(name = "a", type = "Iden", nrow = 3, ncol = 3))@labels

# ========================
# = 2. test on RAM model =
# ========================
manifests = names(xy) # list all the squares in the diagram
m1 <- mxModel("x_predicts_y", type = "RAM", manifestVars = manifests, # add the manifests
    mxPath(from = "x", to = "y"),                         # manifest causes (1 arrow is the default)
    mxPath(from = manifests, to = manifests, arrows = 2), # 2-headed arrows to each manifest for residuals 
    mxPath(from = "one", to = manifests),                 # manifest means
	mxData(xy, type = "raw")
)

omxGetParameters(m1)
m1 = umxLabel(m1)
omxGetParameters(m1)

# =======================================
# = 3. Test on matrix model with submodels =
# =======================================
data(twinData, package = "OpenMx")
selDVs <- c('ht1', 'wt1', 'ht2', 'wt2') # height and weight for each twin
nVar   <- length(selDVs)/2 # number of variables
mzData <- as.matrix(subset(myTwinData, zyg == 1, selDVs)) # female MZ twins
dzData <- as.matrix(subset(myTwinData, zyg == 3, selDVs)) # female DZ twins

ACE <- mxModel("ACE_cholesky",
	mxModel("top",
		# Matrices a, c, and e to store a, c, and e path coefficients
		mxMatrix(name = "a", type = "Lower", nrow = nVar, ncol = nVar, free = T, values = .6),
		mxMatrix(name = "c", type = "Lower", nrow = nVar, ncol = nVar, free = T, values = .2), 
		mxMatrix(name = "e", type = "Lower", nrow = nVar, ncol = nVar, free = T, values = .2), 

		# Matrices A, C, and E compute variance components
		mxAlgebra( a %*% t(a), name = "A" ),
		mxAlgebra( c %*% t(c), name = "C" ),
		mxAlgebra( e %*% t(e), name = "E" ),
		mxAlgebra(A+C+E     , name = "ACE"),
		mxAlgebra(A+C       , name = "AC" ),
		mxAlgebra(.5%x%A + C, name = "hAC"),

		# Matrix & Algebra for expected means vector
		mxMatrix(type = "Full", nrow = 1, ncol = nVar, free = T, values = 1, name = "Mean"),
		mxAlgebra(cbind(Mean, Mean), name = "expMean"),

		# Algebra for expected variance/covariance matrix in MZ
		mxAlgebra(rbind (cbind(ACE, AC), 
		                 cbind(AC , ACE)), dimnames = list(selDVs, selDVs), name="mzCov"),
		mxAlgebra(rbind (cbind(ACE, hAC),
		                 cbind(hAC, ACE)), dimnames = list(selDVs, selDVs), name="dzCov")
	),
    mxModel("MZ",
        mxData(mzData, type = "raw"),
        mxFIMLObjective(covariance = "top.mzCov", means = "top.expMean", dimnames = selDVs)
    ),
    mxModel("DZ",
        mxData(dzData, type = "raw"),
        mxFIMLObjective(covariance = "top.dzCov", means = "top.expMean", dimnames = selDVs)
    ),
    mxAlgebra(MZ.objective + DZ.objective, name="modelfit"),
    mxAlgebraObjective("modelfit")
)
ACE <- mxRun(ACE)
omxGetParameters(ACE) # use this to see what parameters exist in matrices
ACE2 <- omxSetParameters(ACE, "top.a[2,1]", values = 0, free = F, name="no_genetic_covariance")
ACE2 <- mxRun(ACE2)
mxCompare(ACE,ACE2)
umxLabel(ACE)
omxGetParameters(ACE)

umxReportFit(ACE, saturatedModels = ACE_sat, report = "line")

# ========================
# = 4. Inappropriate Inputs =
# ========================
# TODO not fixed yet
umxLabel(1)
# End of unit testing

xmuLabel_MATRIX_Model <- function(model, suffix = "", verbose = T) {
	# Purpose: to label all the free parameters of a (non-RAM) model
	# nb: We don't assume what each matrix is for, and just stick a_r1c1 into each cell
	# Use case: model = xmuLabel_MATRIX_Model(model)
	# Use case: model = xmuLabel_MATRIX_Model(model, suffix = "male")
	if(!is(model, "MxModel")){
		stop("xmuLabel_MATRIX_Model needs model as input")
	}
	if (umxModelIsRAM(model)) {
		stop("xmuLabel_MATRIX_Model shouldn't be seeing RAM Models")
	}
	model = xmuPropagateLabels(model, suffix = "")
	return(model)
}

xmuPropagateLabels <- function(model, suffix = "") {
    # Called by xmuLabel_MATRIX_Model
    # useage: xmuPropagateLabels(model, suffix = "")
	model@matrices  <- lapply(model@matrices , xmuLabel_Matrix, suffix)
    model@submodels <- lapply(model@submodels, xmuPropagateLabels)
    return(model)
}
