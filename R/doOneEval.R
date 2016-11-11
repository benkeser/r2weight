#' .doOneEval
#' 
#' One evaluation of the optimal weight combination and prediction.

.doOneEval <- function(validRows, Y, X, object, seed, return.IC, ...){
    trainY <- Y[-validRows,,drop = FALSE]
    trainX <- X[-validRows,, drop = FALSE]
    validY <- data.matrix(Y[validRows, ,drop = FALSE])
    validX <- X[validRows, , drop = FALSE]
    # fit the optWeight routine
    fit <- optWeight(
        Y = trainY, X = trainX, SL.library = object$SL.library, 
        return.IC = return.IC,
        family = object$SL.family, CV.SuperLearner.V = object$CV.SuperLearner.V, 
        seed = seed, whichAlgorithm = object$whichAlgorithm, 
        return.CV.SuperLearner = FALSE
    )
    # make validation combined outcome
    Y.combn <- validY%*%matrix(fit$SL.weight)
    # make validation combined prediction
    psiHat <- predict(fit, newdata = validX)
    return(cbind(Y.combn, psiHat))
}