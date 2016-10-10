#' Compute a weighted R-squared from CV.SuperLearner objects. 
#' 
#' This function takes a matrix of various outcomes and cross-validated 
#' precitions of these outcomes, along with a set of weights for the outcomes, 
#' and comptues the R-squared for predicting the specified weighted outcome 
#' using (the same) specified weighted combination of predictions. 
#' 
#' @param Ymat A matrix of outcomes on the the full data set with columns 
#' corresopnding to different outcomes. 
#' @param folds A list of the same length as V that was used in the 
#' cross-validation procedure. Each object of the list is a vector specifying
#' the indices of the rows that made up the validation set for that split. 
#' @param pred A list of the same length as V that was used in the 
#' cross-validation procedure containing the predictions made on the training 
#' sample by the prediction algorithms. Each entry should be a matrix with a 
#' column for a prediction of each outcome. 
#' @param validPred A list the same length as V that was used in the 
#' cross-validation procedure containing the predictions made on the validation 
#' sample by the prediction algorithms. Each entry should be matrix with a 
#' column for a prediction of each outcome.
#' @param weightList A list the same length as V that was used in the 
#' cross-validation procedure containing the predictions made on the validation 
#' sample by the prediction algorithms. Should be matrix with a column for a 
#' prediction of each outcome. 
#' @param V The number of sample paritions used in cross-validation procedure.
#' @param J The number of different outcomes. 
#' @param n The number of observations. 
#'
#' @return cv.wR2 An estimated of the weighted R-squared 
#' @return cv.wR2.ci A 95\% confidence interval for the weighted R-squared
#' @return cv.wSummary A data.frame illustrating the weights and R-squared 
#' estimated in each fold
#' @return icList A list of estimated influence functions
#' @return numList A list of estimated cross-validated weighted MSE (numerator 
#' of R-squared)
#' @return denomList A list of estimated variances of weighted outcome 
#' (denominator of R-squared)
#' @return n The sample size
#' @return V The number of partitions
#' @return foldnList A list of the number in each partition
#'   
#' @export

computeR2 <- function(Ymat, folds, pred, validPred, weightList, V, J, n){
    # get honest cv-r2 estimates
    # alpha.Pnv0 = weights derived from training sample
    # Pnv0 = list with Y and psiHat from training sample
    # Pnv1 = list with Y and psiHat from validation sample
    .getHonestR2 <- function(alpha.Pnv0, Pnv0, Pnv1){
        # weighted risk for Pnv1
        Pnv1.L.psiHat <- mean(((Pnv1$Y - Pnv1$psiHat)%*%alpha.Pnv0)^2)
        
        # mean of Y in training sample of dimension of validation sample
        Pnv0.Y1 <- matrix(rep(apply(Pnv0$Y,2,mean), rep(nrow(Pnv1$Y),J)),ncol=J)
        # weighted "variances" using Pnv0(Y) computed on validation sample
        Pnv1.L.yBar <- mean(((Pnv1$Y - Pnv0.Y1)%*%alpha.Pnv0)^2)
        
        # return honest R^2 estimate
        return(c(Pnv1.L.psiHat , Pnv1.L.yBar))
    }
    
    # function to compute influence curves
    # returns matrix of influences curves at Pnv0 
    # evaluated on validation sample 
    .getInfluenceCurve <- function(alpha.Pnv0, Pnv0, Pnv1){
        # mean of Y in training sample of dimension of training sample
        Pnv0.Y0 <- matrix(rep(apply(Pnv0$Y,2,mean), rep(nrow(Pnv0$Y),J)),ncol=J)
        # mean of Y in training sample of dimension of training sample
        Pnv0.Y1 <- matrix(rep(apply(Pnv0$Y,2,mean), rep(nrow(Pnv1$Y),J)),ncol=J)
        
        
        # IC(Pnv0) at observations in training sample
        IC1v.Pnv0 <- ((Pnv1$Y - Pnv1$psiHat)%*%alpha.Pnv0)^2 - 
            mean(((Pnv0$Y - Pnv0$psiHat)%*%alpha.Pnv0)^2)
        
        IC2v.Pnv0 <- ((Pnv1$Y - Pnv0.Y1)%*%alpha.Pnv0)^2 - 
            mean(((Pnv0$Y - Pnv0.Y0)%*%alpha.Pnv0)^2)
        
        ICv.Pnv0 <- matrix(c(IC1v.Pnv0, IC2v.Pnv0), nrow=2, byrow=TRUE)
    }
    
    # this function computes Pnv1(IC1v.Pnv0/psi.10n - IC1v.Pnv0/psi.20n)^2,
    # i.e., the estimated variance of log(honestRisk/honestVariance)
    # for the v-th split
    .getSigma2v <- function(ic, honestRisk, honestVariance){
        mean((ic[1,]/honestRisk - ic[2,]/honestVariance)^2)
    }
    
    # get honest weighted R2 over folds
    honestR2List <- lapply(split(1:V,1:V), function(v){
        .getHonestR2(
            alpha.Pnv0 = weightList[[v]],
            Pnv0 = list(Y = matrix(Ymat[-folds[[v]],],ncol=J), 
                        psiHat = matrix(pred[[v]],ncol=J)),
            Pnv1 = list(Y = matrix(Ymat[folds[[v]],],ncol=J), 
                        psiHat = matrix(validPred[[v]],ncol=J))
        )
    })
    
    honestR2Matrix <- Reduce(rbind, honestR2List)
    honestRisk <- mean(honestR2Matrix[,1])
    honestVariance <- mean(honestR2Matrix[,2])
    honestR2 <- 1 - honestRisk/honestVariance
    
    icList <- lapply(split(1:V,1:V), FUN=function(v){
        .getInfluenceCurve(
            alpha.Pnv0 = weightList[[v]],
            Pnv0 = list(Y = matrix(Ymat[-folds[[v]],],ncol=J), psiHat = matrix(pred[[v]],ncol=J)),
            Pnv1 = list(Y = matrix(Ymat[folds[[v]],],ncol=J), psiHat = matrix(validPred[[v]],ncol=J))
        )
    })
    
    # apply it over the splits
    sigma2List <- lapply(icList, FUN=.getSigma2v, honestRisk=honestRisk, 
                         honestVariance = honestVariance)
    # take the average over the splits
    sigma2n <- Reduce(mean,sigma2List)
    # divide by n and take square root to get a std error estimator
    seLogHonestR2 <- sqrt(sigma2n/n)
    
    # compute confidence interval on re-transformed scale
    logHonestR2 <- log(1-honestR2)
    honestR2Low <- 1 - exp(logHonestR2 + 1.96*seLogHonestR2)
    honestR2High <- 1 - exp(logHonestR2 - 1.96*seLogHonestR2)
    
    foldnList <- lapply(folds,length)
    
    # a summary data frame that shows R^2 for each fold
    # and the weights for each fold
    suppressWarnings(
        summV <- data.frame(w.R2=round(1-unlist(honestR2Matrix[,1]/honestR2Matrix[,2]),3),
                            round(Reduce(rbind,weightList),3),check.names=FALSE)
    )
    names(summV)[2:(J+1)] <- paste0("w",1:J)
    
    # format output
    out <- list(
        cv.wR2 = honestR2,
        cv.wR2.ci = c(honestR2Low, honestR2High),
        cv.wSummary = summV,
        icList = icList,
        numList = honestR2Matrix[,1],
        denomList = honestR2Matrix[,2], n=n, V=V,
        foldnList=foldnList
    )
    return(out)
}
