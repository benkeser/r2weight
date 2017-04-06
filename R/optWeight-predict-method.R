#' predict.optWeight
#' 
#' A method for prediction of weighted outcome based on \code{optWeight} fitted object.
#' 
#' @param object A fit \code{optWeight} object. 
#' @param newdata A new data frame with the same design as the original \code{X} object
#' that was used in the call to \code{optWeight}.
#' @param ... Other args (not used)
#' @export 
#' @importFrom stats predict


predict.optWeight <- function(object, newdata,...){
    pred <- Reduce("cbind",lapply(object$SL.fit, function(o){
        p <- stats::predict(o, newdata = newdata)
        if(object$whichAlgorithm == "SuperLearner"){
            p[[1]]
        }else if(object$whichAlgorithm == "discreteSuperLearner"){
            p[[2]][,which(o$cvRisk == min(o$cvRisk,na.rm = TRUE))]
        }else{
            p[[2]][,object$whichAlgorithm]
        }
    }))
    pred%*%matrix(object$SL.weights)
}