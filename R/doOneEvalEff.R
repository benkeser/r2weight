#' .doOneEvalEff
#' 
#' One evaluation of the optimal weight combination and predictions for treatment effect.
#' 
#' @param validRows List of validation rows indices
#' @param Y The outcome
#' @param X The covariates
#' @param Z The binary treatment
#' @param seed The random seed to set
#' @param SuperLearner.V The number of folds for each Super Learner
#' @param SL.library.g The library for propensity score
#' @param select How to choose amongst outcomes
#' @param parallel Boolean to indicate running in parallel
#' @param n.cores Number of cores to use
#' @param family Family for the outcome
#' 
#' 

.doOneEvalEff <- function(validRows, Y, X, Z, seed, SuperLearner.V,
                          SL.library.Q, SL.library.g, select, 
                          parallel, n.cores, family, ...){
  
    trainYmat <- data.matrix(Y[-validRows,,drop = FALSE])
    trainX <- X[-validRows,, drop = FALSE]
    trainZ <- Z[-validRows]
    validYmat <- data.matrix(Y[validRows, ,drop = FALSE])
    validX <- X[validRows, , drop = FALSE]
    validZ <- Z[validRows]
    
    # fit SL for each outcome in training sample
    SuperLearner.list <- apply(trainYmat, 2, function(y){
        set.seed(seed)
        fit <- SuperLearner::SuperLearner(
            Y = y, X = data.frame(trainX, Z = trainZ), SL.library = SL.library.Q, family = family, 
            cvControl = list(V=SuperLearner.V)
        )
    })
    
    # fit SL for propensity
    SuperLearner.g <- SuperLearner::SuperLearner(
        Y = trainZ, X = trainX, SL.library = SL.library.Q, family = "binomial", 
        cvControl = list(V = SuperLearner.V)
    )
    
    # get predictions for training sample
    trainQn1Mat <- Reduce("cbind",lapply(SuperLearner.list, function(x){
        stats::predict(x, newdata = data.frame(trainX, Z = 1))[[1]]
    }))
    trainQn0Mat <- Reduce("cbind",lapply(SuperLearner.list, function(x){
        stats::predict(x, newdata = data.frame(trainX, Z = 0))[[1]]
    }))
    
    # get propensity on training sample
    traingn <- SuperLearner.g$SL.pred
    
    # get weights
    alpha_n <- alphaHatEff(Y = trainYmat, Z = trainZ, X = trainX, Qn1 = trainQn1Mat, 
                         Qn0 = trainQn0Mat, gn = traingn, select = select)
    
    # get combined stats::predictions in validation sample
    validQn1Mat <- Reduce("cbind",lapply(SuperLearner.list, function(x){
        stats::predict(x, newdata = data.frame(validX, Z = 1))[[1]]
    }))
    validQn1Combn <- validQn1Mat%*%alpha_n
    
    validQn0Mat <- Reduce("cbind",lapply(SuperLearner.list, function(x){
        stats::predict(x, newdata = data.frame(validX, Z = 0))[[1]]
    }))
    validQn0Combn <- validQn0Mat%*%alpha_n
    
    validgn <- stats::predict(SuperLearner.g, data = validX)[[1]]
    
    # get combined outcomes in validation sample
    validYcombn <- validYmat%*%alpha_n
    
    return(
        list(Ycombn = validYcombn, 
             Qn1Combn = validQn1Combn, 
             Qn0Combn = validQn0Combn,
             gn = validgn,
             alpha_n = alpha_n)
    )
}