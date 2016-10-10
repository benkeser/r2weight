#' Compute the weights used by r2weight 
#' 
#' This function takes a list called P that consists of: Y, a matrix with 
#' columns corresponding to different outcomes in a particular partition; and 
#' psiHat, a matrix of predictions from CV.SuperLearner on the same partition of 
#' the data as the matrix Y, with columns again corresponding to different 
#' outcomes. The function computes the set of weights that maximize the value
#' of R-squared in this partition using solnp() from the Rsolnp package. 
#' 
#' 
#' @param P A list called P that consists of: Y, a matrix with columns 
#' corresponding to different outcomes in a particular partition; and 
#' psiHat, a matrix of predictions from CV.SuperLearner on the same partition of 
#' the data as the matrix Y, with columns again corresponding to different 
#' outcomes.  \code{P}

#' @return weights A numeric vector of weights of the same length as the number
#' of outcomes considered. The weights sum to 1. 

alphaHat <- function(P){
    J <- ncol(P$Y)
    
    # constraint function to force weights to sum to 1
    constraint <- function(alpha.P, Pnv0){
        1-sum(alpha.P)
    }
    
    # function that computes R^2(P)
    # alpha.P = the weights that solnp will maximize over
    # Pnv0 = list with entries described above
    getMinusR2.Pnv0 <- function(alpha.P, Pnv0){
        J <- ncol(Pnv0$Y)
        # weighted risk for this P
        Pnv0.L.psiHat <- mean(((Pnv0$Y - Pnv0$psiHat)%*%alpha.P)^2)
        # weighted variances for this P
        Pnv0.Y <- matrix(rep(apply(Pnv0$Y,2,mean), rep(nrow(Pnv0$Y),J)),ncol=J)
        Pnv0.L.yBar <- mean(((Pnv0$Y - Pnv0.Y)%*%alpha.P)^2)
        # return 1-R^2.P because solnp minimizes
        return(Pnv0.L.psiHat / Pnv0.L.yBar)
    }
    
    # minimize 1-R^2(P) subject to constraint
    fm <- Rsolnp::solnp(pars=rep(1/J,J), fun=getMinusR2.Pnv0, LB=rep(0,J), UB=rep(1,J), 
                eqfun=constraint, control=list(trace=0), eqB=0, 
                Pnv0=P)
    # return weights
    return(fm$pars)
}