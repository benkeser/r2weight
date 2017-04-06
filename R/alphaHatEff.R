#' alphaHatEff
#' 
#' Compute weights for ate estimation
#' 
#' 
#' @param Y The outcome
#' @param X The covariates
#' @param Z The binary treatment
#' @param Qn1 A matrix of predictions from SuperLearner on training data with Z = 1
#' @param Qn0 A matrix of predictions from SuperLearner on training data with Z = 0
#' @param gn A matrix of propensity estimates in training data
#' @param select How to choose amongst outcomes
#' 
#' 
#' @return weights A numeric vector of weights of the same length as the number
#' of outcomes considered. The weights sum to 1. 
#' 
#' @importFrom tmle tmle
#' @importFrom Rsolnp solnp


alphaHatEff <- function(Y, X, Z, Qn1, Qn0, gn, select){
    J <- ncol(Y)
    
    # constraint function to force weights to sum to 1
    constraint <- function(alpha_n, Y, X, Z, Qn1, Qn0, gn, select){
        1-sum(alpha_n)
    }
    
    # function that computes R^2(P)
    # alpha.P = the weights that solnp will maximize over
    # Pnv0 = list with entries described above
    if(select == "p-value"){
        getATE <- function(alpha_n, Y, X, Z, Qn1, Qn0, gn){
            J <- ncol(Y)
            # weighted outcome
            Ycombn <- Y%*%alpha_n
            
            # weighted predictions
            Qn1Combn <- Qn1%*%alpha_n
            Qn0Combn <- Qn0%*%alpha_n
            
            # tmle fit
            fit.tmle <- tmle::tmle(Y = Ycombn, A = Z, W = X, Q = cbind(Qn0Combn, Qn1Combn), g1W = gn)
            
            # return 
            return(
                -abs(fit.tmle$estimates$ATE$psi/sqrt(fit.tmle$estimates$ATE$var.psi))
            )
        }
        
        # minimize 1-R^2(P) subject to constraint
        fm <- Rsolnp::solnp(pars=rep(1/J,J), fun=getATE, LB=rep(0,J), UB=rep(1,J), 
                            eqfun=constraint, control=list(trace=0), eqB=0, 
                            Y=Y, X=X, Z=Z, Qn1=Qn1, Qn0=Qn0, gn = gn)
        wt <- fm$pars
    }else if(select == "effect"){
        ateVec <- rep(NA, J)
        for(j in 1:J){
            fit.tmle <- tmle::tmle(Y = Y[,j], A = Z, W = X, Q = cbind(Qn0[,j], Qn1[,j]), g1W = gn)
            ateVec[j] <- -abs(fit.tmle$estimates$ATE$psi)
        }
        wt <- rep(0, J); biggestATE <- which(ateVec == min(ateVec))
        wt[biggestATE] <- 1
    }
    # return weights
    return(wt)
}