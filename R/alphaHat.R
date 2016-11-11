#' alphaHat
#' 
#' This function takes a list called P that consists of: Y, a matrix with 
#' columns corresponding to different outcomes in a particular partition; and 
#' psiHat, a matrix of predictions from CV.SuperLearner on the same partition of 
#' the data as the matrix Y, with columns again corresponding to different 
#' outcomes. The function computes the set of weights that maximize the value
#' of R-squared in this partition using solnp() from the Rsolnp package. 
#' 
#' 
#' @param Y A matrix with columns corresponding to different outcomes 
#' @param psiHat.Pnv0 A matrix of predictions from CV.SuperLearner on the validation data
#' 
#' @return weights A numeric vector of weights of the same length as the number
#' of outcomes considered. The weights sum to 1. 


alphaHat <- function(Y, psiHat.Pnv0){
    J <- ncol(Y)
    
    # constraint function to force weights to sum to 1
    constraint <- function(alpha_n, Y, psiHat.Pnv0){
        1-sum(alpha_n)
    }
    
    # function that computes R^2(P)
    # alpha.P = the weights that solnp will maximize over
    # Pnv0 = list with entries described above
    getMinusR2_n <- function(alpha_n, Y, psiHat.Pnv0){
        J <- ncol(Y)
        # weighted risk for this P
        Pnv1.MSE.psiHat.Pnv0 <- mean(((Y - psiHat.Pnv0)%*%alpha_n)^2)
        # weighted variances for this P
        PnY <- matrix(rep(apply(Y,2,mean), rep(nrow(Y),J)),ncol=J)
        Pn.MSE.PnY <- mean(((Y - PnY)%*%alpha_n)^2)
        # return 1-R^2.P because solnp minimizes
        return(Pnv1.MSE.psiHat.Pnv0 / Pn.MSE.PnY)
    }
    
    # minimize 1-R^2(P) subject to constraint
    fm <- Rsolnp::solnp(pars=rep(1/J,J), fun=getMinusR2_n, LB=rep(0,J), UB=rep(1,J), 
                eqfun=constraint, control=list(trace=0), eqB=0, 
                Y=Y, psiHat.Pnv0 = psiHat.Pnv0)
    # return weights
    return(fm$pars)
}