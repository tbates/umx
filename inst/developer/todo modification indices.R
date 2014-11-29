genEpiMI <- function(model, vector=T) {
    accumulate <- function(A, B, C, D, d) {
        res <- matrix(0, d^2, d^2)    
        for (ii in 1:(d^2)){
            for (jj in ii:(d^2)){
                g <- 1 + (ii - 1) %% d
                h <- 1 + (ii - 1) %/% d
                i <- 1 + (jj - 1) %% d
                j <- 1 + (jj - 1) %/% d
                res[ii, jj] <- res[jj, ii] <- A[g, i] * B[h, j] + C[g, j] * D[h, i]
			}
		}
        res
    }
    accumulate.asym <- function(A, B, C, D, d) {
        res <- matrix(0, d^2, d^2)    
        for (ii in 1:(d^2)){
            for (jj in 1:(d^2)){
                g <- 1 + (ii - 1) %% d
                h <- 1 + (ii - 1) %/% d
                i <- 1 + (jj - 1) %% d
                j <- 1 + (jj - 1) %/% d
                res[ii, jj] <- A[g, i] * B[h, j] + C[g, j] * D[h, i]
			}
        }
    	res
    }        
    A <- model$A@values
    P <- model$S@values
    S <- model$data@observed
    J <- model$F@values
    m <- dim(A)[1]
    which.free <- c(model$A@free, model$S@free & upper.tri(diag(m), diag=T))
    vars <- colnames(A)
    parNames <- c(model$A@labels, model$S@labels)
    parNames[is.na(parNames)] <- c(outer(vars, vars, paste, sep=' <- '),
                   outer(vars, vars, paste, sep=' <-> '))[is.na(parNames)]
    NM <- model$data@numObs - 1
    I.Ainv <- solve(diag(m) - A) 
    C <- J %*% I.Ainv %*% P %*% t(I.Ainv) %*% t(J)
    Cinv <- solve(C)    
    AA <- t(I.Ainv) %*% t(J)
    BB <- J %*% I.Ainv %*% P %*% t(I.Ainv)
    correct <- matrix(2, m, m)
    diag(correct) <- 1
    grad.P <- correct * AA %*% Cinv %*% (C - S) %*% Cinv %*% t(AA)
    grad.A <- correct * AA %*% Cinv %*% (C - S) %*% Cinv %*% BB 
    grad <- c(grad.A, grad.P) * NM
    names(grad) <- parNames
    dF.dBdB <- accumulate(AA %*% Cinv %*% t(AA), t(BB) %*% Cinv %*% BB,
                    AA %*% Cinv %*% BB, t(BB) %*% Cinv %*% t(AA), m)
    dF.dPdP <- accumulate(AA %*% Cinv %*% t(AA), AA %*% Cinv %*% t(AA),
                    AA %*% Cinv %*% t(AA), AA %*% Cinv %*% t(AA), m)
    dF.dBdP <- accumulate.asym(AA %*% Cinv %*% t(AA), t(BB) %*% Cinv %*% t(AA),
                    AA %*% Cinv %*% t(AA), t(BB) %*% Cinv %*% t(AA), m)
    correct.BB <- correct.PP <- correct.BP <- matrix(1, m^2, m^2)
    correct.BB[diag(m)==0, diag(m)==0] <- 2
    correct.PP[diag(m)==1, diag(m)==1] <- 0.5
    correct.PP[diag(m)==0, diag(m)==0] <- 2
    correct.BP[diag(m)==0, diag(m)==0] <- 2
    Hessian <- NM*rbind(cbind(dF.dBdB * correct.BB,    dF.dBdP * correct.BP),
                        cbind(t(dF.dBdP * correct.BP), dF.dPdP * correct.PP))
    rownames(Hessian) <- parNames
    colnames(Hessian) <- parNames
    # list(gradient=grad[which.free], Hessian[which.free, which.free])

    hessian <- Hessian[which.free, which.free]
    E.inv <- solve(hessian)                      
    par.change <- mod.indices <- rep(0, 2*(m^2))                
    for (i in 1:(2*(m^2))) {
        k <- Hessian[i, i]
        d <- Hessian[i, which.free]
        par.change[i] <- -grad[i] / (k - d %*% E.inv %*% d)
        mod.indices[i] <- -0.5 * grad[i] * par.change[i]
	}
    names(mod.indices) <- parNames
    names(par.change)  <- parNames
    if (vector) {
      which.ret <- c(!model$A@free & !diag(m), !model$S@free) # & upper.tri(diag(m), diag=T))
      sel <- order(mod.indices[which.ret], decreasing=T)
      ret <- list(mi=mod.indices[which.ret][sel], par.change=par.change[which.ret][sel])
    } else {
      mod.A <- matrix(mod.indices[1:(m^2)], m, m)
      mod.P <- matrix(mod.indices[-(1:(m^2))], m, m)
      par.A <- matrix(par.change[1:(m^2)], m, m)
      par.P <- matrix(par.change[-(1:(m^2))], m, m)
      rownames(mod.A) <- colnames(mod.A) <- vars
      rownames(mod.P) <- colnames(mod.P) <- vars
      rownames(par.A) <- colnames(par.A) <- vars
      rownames(par.P) <- colnames(par.P) <- vars
      mod.A[model$A@free] <- NA
      par.A[model$A@free] <- NA
      diag(mod.A) <- NA
      diag(par.A) <- NA
      mod.P[model$S@free] <- NA
      par.P[model$S@free] <- NA
      ret <- list(mod.A=mod.A, par.A=par.A, mod.S=mod.P, par.S=par.P)
    }
    ret
}


require(OpenMx)
table4 <- matrix(nrow = 7, byrow = T, data=c(
	   1,     0,     0,     0,     0,     0,     0,
   0.150,     1,     0,     0,     0,     0,     0,
   0.210, 0.350,     1,     0,     0,     0,     0,
   0.036, 0.060, 0.084,     1,     0,     0,     0,
   0.072, 0.120, 0.168, 0.580,     1,     0,     0,
   0.108, 0.180, 0.252, 0.520, 0.540,     1,     0,
   0.144, 0.240, 0.336, 0.160, 0.320, 0.480,     1)
)

table4 <- table4 + t(table4)
diag(table4) <- 1
rownames(table4) <- paste("x", 1:7, sep = "")
colnames(table4) <- rownames(table4)
manifests <- rownames(table4)
latents   <- c("f1", "f2")
fit1      <- mxModel(name = "page378",type = "RAM",
   manifestVars = manifests,
   latentVars   = latents,
   mxPath(from = manifests, arrows = 2, values=1, labels = c(paste("theta", 1:7, sep = ""))),
   mxPath(from = "f1", to = manifests[1:3], labels = paste("lambda1", 1:3, sep = "")),
   mxPath(from = "f2", to = manifests[4:7], labels = paste("lambda2", 4:7, sep = "")),
   mxPath(from = "f1", to = "f2", arrows = 2, labels = "phi"),
   mxPath(from = latents, arrows = 2, free = F, values = 1.0),
   mxData(observed = table4, type = "cov", numObs = 1000)
)

fit1    <- mxRun(fit1)
fit1.mi <- genEpiMI(fit1)

fit2    <- mxModel(fit1, name='add45',mxPath(from='x4', to='x5', arrows=2, free=T, values=0))
fit2    <- mxRun(fit2)
fit2.mi <- genEpiMI(fit2)

fit3    <- mxModel(fit2, name='add71', mxPath(from='f1', to='x7', arrows=1, free=T, values=0))
fit3    <- mxRun(fit3)
fit3.mi <- genEpiMI(fit3)

sink('sorbom.txt')
summary(fit1)$Chi
summary(fit2)$Chi
summary(fit3)$Chi

fit.mi$mi['x4 <-> x5']
fit2.mi$mi['x7 <- f1']
fit3.mi$mi['x4 <-> x6']
sink()