#' getUnivariateR2
#' 
#' Comptues the R^2 and a confidence interval for a particular outcome and 
#' Super Learner prediction.
#' 
#'  @inheritParams r2weight::alphaHat
#'  @param Y The matrix of outcomes
#'  
#' 
#' @return A \code{list} with point estimate and 95% Wald-style confidence interval 
#' for R^2 for each outcome individually. 
#' 

# TO DO: Add option for confidence interval level. 

getUnivariateR2 <- function(Y, psiHat.Pnv0, return.IC){
    n <- length(Y[,1])
    J <- ncol(Y)
    
    # compute Y bar for each outcome
    Ybar <- colMeans(Y)
    
    # MSE for each outcome
    MSE <- apply(matrix(1:J), 1, function(j){
        mean((Y[,j] - psiHat.Pnv0[,j])^2)
    })
    
    # ic for MSE for each outcome 
    IC.MSE <- apply(matrix(1:J), 1, function(j){
        (Y[,j] - psiHat.Pnv0[,j])^2 - MSE[j]
    })

    # Var for each otucome
    Var <- apply(matrix(1:J), 1, function(j){
        mean((Y[,j] - Ybar[j])^2)
    })
    
    # ic for Variance -- denominator
    IC.Var <- apply(matrix(1:J), 1, function(j){
        (Y[,j] - Ybar[j])^2 - Var[j]
    })
    
    # ic for log(numerator) - log(denominator)
    se.logR2 <- apply(matrix(1:J),1,function(j){
        grad <- matrix(c(1/MSE[j],-1/Var[j]),nrow=1)
        IC <- cbind(IC.MSE[,j],IC.Var[,j])
        sqrt(grad%*%t(IC)%*%IC%*%t(grad))/n
    })
    
    # point est + 95% CI
    out <- lapply(split(1:J,1:J),function(j){
        est <- 1 - MSE[j]/Var[j]
        ci.low <- 1 - exp(
            log(MSE[j]/Var[j]) + 1.96*se.logR2[j]
        )
        ci.high <- 1 - exp(
            log(MSE[j]/Var[j]) - 1.96*se.logR2[j]
        )
        return(c(est, ci.low, ci.high))
    })
    names(out) <- colnames(Y)
    out$MSE <- MSE
    out$Var <- Var
    out$IC <- NULL
    if(return.IC){
        out$IC <- list(IC.MSE = IC.MSE, IC.Var = IC.Var)
    }
    return(out)
}