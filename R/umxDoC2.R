umxDoC2 <- function (name = "DoC", var1Indicators, var2Indicators,covar=NULL, mzData = NULL, 
                       dzData = NULL, sep = "_T", causal = FALSE, autoRun = getOption("umx_auto_run"), 
                       intervals = FALSE, tryHard = c("no", "yes", "ordinal", "search"), 
                       optimizer = NULL, data = NULL, zyg = "zygosity", batteries = c("scale","ordinaloptim","upcovar"), verbose = FALSE) 
  {
    colTypes = NULL
    colTypes$nOrdVars = 0
   ordinalPresent = FALSE
    selVars = tvars(c(var1Indicators, var2Indicators), sep = sep)


    if ("ordinaloptim" %in% batteries & !missing(data) ) {
    # get all columns from data that are ordinal
    if (umx_is_ordered(xmu_extract_column(data, selVars),
                              summaryObject= TRUE)$nOrdVars >0) {
			data = xmu_relevel_factors(data, cols = selVars , min = 7)
    }
      }
  
    if (name == "DoC") {
      name = ifelse(causal, "DoC", "Chol")
    }
    tryHard = match.arg(tryHard)
    umx_check(is.logical(causal), "stop", "causal must be TRUE or FALSE")

    if (!is.null(data)) {
      if ("tbl" %in% class(data)) {
        data = as.data.frame(data)
      }
      if ("scale" %in% batteries) {
        data = umx_scale_wide_twin_data(data=data, varsToScale = c(var1Indicators, var2Indicators),sep=sep, twins=1:2)
      } 

      mzData = data[data[, zyg] %in% ifelse(is.null(mzData), "MZ", mzData), ]
      dzData = data[data[, zyg] %in% ifelse(is.null(dzData), "DZ", dzData), ]

    } else {
      if ("tbl" %in% class(mzData)) {
        mzData = as.data.frame(mzData)
        dzData = as.data.frame(dzData)
      }
    }

  if ("upcovar" %in% batteries & !missing(covar)) {
    if (verbose) warning("Updating covariates!")
     xmu_update_covar(data, covar, c(var1Indicators, var2Indicators))
  }

   nSib = 2
    nLat = 2
    nLat1 = length(var1Indicators)
    nLat2 = length(var2Indicators)
    nVar = nLat1 + nLat2
    ntv = nVar*2
  nCov = length(covar)
    indVar <- paste(covar,c(rep(1,nCov),rep(2,nCov)),sep=sep)

   colTypes = umx_is_ordered(xmu_extract_column(mzData, selVars),
                              summaryObject= TRUE)

    # Create data objects
    mzData = xmu_make_mxData(mzData, manifests = selVars, fullCovs = indVar)
    dzData = xmu_make_mxData(dzData, manifests = selVars, fullCovs = indVar)

      xmu_twin_check(selDVs = c(var1Indicators, var2Indicators), 
                   sep = sep, dzData = dzData, mzData = mzData, enforceSep = TRUE, 
                   nSib = nSib, optimizer = optimizer)


    ## Λ FacLoad - factor loadings from the latent true scores or common pathways to the observed variables
    FacLoad     <- umxMatrix("FacLoad",type="Full", nrow=nVar, ncol=nLat)
    FacLoad$free[1:nLat1, 1] = TRUE
    FacLoad$values[1:nLat1, 1] = 1
    FacLoad$free[(nLat1 + 1):(nLat1 + nLat2), 2] = TRUE
    FacLoad$values[(nLat1 + 1):(nLat1 + nLat2), 2] = 1
    FacLoad$lbound[1:nLat1, 1] = .00001 
    FacLoad$lbound[(nLat1 + 1):(nLat1 + nLat2), 2] = .00001

    ## ϵ epsilon - measurement error/residuals or transient variance in each observed/manifest item/variable (not explained by the latent true scores)
    ## Expected covariance for mutiple indicator DOC model
    ## Λ * (I-Β)~ * Ψ * (I-Β)~' * Λ' + ϵ
    ## Λ = FacLoad - factor loadings
    ## I = Identity matrix
    ## Β = causal parameters
    ## ϵ = epsilon - measurement error/residuals

    ## Standardized variance Components
    rowVC      <- rep('VC', nVar)
    colVC      <- rep(c('A','C','E','SA','SC','SE'),each=nVar)
    ## Latent true score means for autoregression component
    ## Mean matrix = μ
    ## Factor mean = ((I-Β)~ * μ)'   Adjusts each mean for contribution from causal pathway
    ## Manifest mean = (Λ * ( (I-Β)~ * μ ))'  

if (colTypes$nOrdVars > 0){
        ty = umxThresholdMatrix( rbind(mzdata), fullVarNames = colTypes$factorVarNames, #,dzdata
                            sep = "_T", method="Mehta")
        ordinalPresent = TRUE

 objMZ       <- mxExpectationNormal( covariance="expCovMZ", means="expMean", dimnames=selVars ,
      thresholds="top.threshMat",
       threshnames = colTypes$factorVarNames)

    objDZ       <- mxExpectationNormal( covariance="expCovDZ", means="expMean", dimnames=selVars,
     thresholds="top.threshMat",
              threshnames = colTypes$factorVarNames)
      } else {
    objMZ       <- mxExpectationNormal( covariance="expCovMZ", means="expMean", dimnames=selVars )
    objDZ       <- mxExpectationNormal( covariance="expCovDZ", means="expMean", dimnames=selVars)
      }
      ## Function to compute -2*(log likelihood) of the data given the current values of the free parameters and the expectation function

    ## My part
  if (!missing(covar)) {
   dCov1 <- mxMatrix( type = "Full", nrow =nCov , ncol = 1, free = FALSE,
                      labels = paste("data.", paste(c(rep(covar)), c(1), sep = "_T"), sep = ""),
                      name = "dCov1" )

    dCov2 <- mxMatrix( type = "Full", nrow = nCov, ncol = 1, free = FALSE,
                      labels = paste("data.", paste(c(rep(covar)), c(2), sep = "_T"), sep = ""),
                      name = "dCov2" )
    expMean<-mxAlgebra(name = "expMean", cbind(  top.ManMean +t(top.bCov%*%dCov1),
                                               top.ManMean +t(top.bCov%*%dCov2)))
  } else {
    expMean  	<- mxAlgebra( expression= cbind(top.ManMean,top.ManMean), name="expMean" ) 
  } 


    top <- mxModel("top", FacLoad, 
                   mxAlgebra(name="VC", expression=cbind(A,C,E,A/V,C/V,E/V), dimnames=list(rowVC,colVC)),
                   ## Constrain the variance components for common pathways
                   mxMatrix(name="unitM", type="Unit", nrow=2, ncol=1),
                   mxConstraint(name="ConVarar",  expression=diag2vec( (solve(I-beta) %&% psi_a) +
                                                                       (solve(I-beta) %&% psi_c) +
                                                                       (solve(I-beta) %&% psi_e) )==unitM), 
                   mxAlgebra(name="V", expression= A+C+E),
                   mxMatrix(name = "Mean", type="Full", nrow=2, ncol=1, free=c(T,T),
                            labels=c( "m1","m2"), values = c(0.01,0.30)),
                   mxAlgebra(name="FacMean",solve(I-beta)  %*% Mean ), 
                   mxAlgebra(name="ManMean",t(FacLoad %*% FacMean ) ),      
                   mxAlgebra(name = "A",FacLoad  %&% (solve(I-beta) %&% psi_a) + as), #
                   mxAlgebra(name = "C",FacLoad  %&% (solve(I-beta) %&% psi_c)+ cs ), #
                   mxAlgebra(name = "E", FacLoad %&% (solve(I-beta) %&% psi_e) + es),
                   umxMatrix("as",type = "Diag", nrow=nVar, ncol=nVar,  free=T, values=0.3), 
                   umxMatrix("cs",type = "Diag", nrow=nVar, ncol=nVar,  free=T, values=0.2), 
                   umxMatrix("es",type = "Diag", nrow=nVar, ncol=nVar,  free=T, values=0.1), 
                   umxMatrix("beta",type="Full", nrow=nLat, ncol=nLat, labels=c(NA,"a2b","b2a",NA), free=F, values=0, lbound=-1, ubound=1 ),
                   umxMatrix("I",type="Iden", nrow=nLat, ncol=nLat),
                   umxMatrix("psi_a",type = "Lower", nrow = nLat,ncol=nLat,  free=T, values=.2),
                   umxMatrix("psi_c",type = "Lower", nrow = nLat,ncol=nLat,  free=T, values=.2), 
                   umxMatrix("psi_e",type = "Lower", nrow = nLat,ncol=nLat, free=T, values=1)) #, ty


    MZ  <- mxModel( "MZ", mzData,
                     mxAlgebra(name="expCovMZ",
                               expression= rbind( cbind(top.A+top.C+top.E, top.A+top.C),
                                                 cbind(top.A+top.C, top.A+top.C+top.E)) ), objMZ,  
                     mxFitFunctionML()
                    ,expMean )  

    DZ  <- mxModel( "DZ", dzData,
                     mxAlgebra(name="expCovDZ", expression= rbind( cbind(top.A+top.C+top.E, 0.5%x%top.A+top.C), cbind(0.5%x%top.A+top.C, top.A+top.C+top.E))), objDZ,   mxFitFunctionML(),expMean )  

  if (!missing(covar)) {
      bCov <-  umxMatrix("bCov", type = "Full",
                             nrow = length(c(var1Indicators,var2Indicators)), ncol = nCov,
                             free = T,
                             values = 0.1, 
                             labels = c(
paste0("b_", covar[1], var1Indicators),
paste0("b_", covar[2], var2Indicators)))
    top = mxModel(top, bCov)
MZ = mxModel(MZ ,dCov1, dCov2)
DZ = mxModel(DZ ,dCov1, dCov2)
  }

      if(colTypes$nOrdVars > 0){
            MZ = MZ + mxExpectationNormal( covariance="expCovMZ", means="expMean", dimnames=selVars ,
                                           thresholds="top.threshMat",
                                           threshnames = colTypes$factorVarNames)

            DZ = DZ + mxExpectationNormal( covariance="expCovDZ", means="expMean", dimnames=selVars,
                                  thresholds="top.threshMat",
                                           threshnames = colTypes$factorVarNames)
      }


      if (ordinalPresent)   top = mxModel(top, ty)

    if (causal) {
      top = mxModel(top, 
                    umxMatrix("psi_a",type = "Symm", nrow = nLat,ncol=nLat,
                              free=c(T,F,F,T), values=c(0.6,0,0,0.6)),
                    umxMatrix("psi_c",type = "Symm", nrow = nLat,ncol=nLat,
                              free=c(T,F,F,T), values=c(0.2,0,0,0.3)), 
                    umxMatrix("psi_e", type = "Symm", nrow = nLat,ncol=nLat,
                              free=c(T,F,F,T), values=c(0.6,0,0,0.6)) 
                    )
    }

    # Combine models
    model = mxModel(name, top, MZ, DZ, mxFitFunctionMultigroup(c("MZ", "DZ")))
    model = omxAssignFirstParameters(model)
    if (missing(covar)) model = mxAutoStart(model)
    model = as(model, "MxModelDoC")
    model = xmu_safe_run_summary(model, autoRun = autoRun, tryHard = tryHard, std = TRUE, summary = FALSE)
    return(model)
  }

