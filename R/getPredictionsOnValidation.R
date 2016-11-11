#' getPredictionsOnValidation
#'
#' Get validation predictions from a CV.SuperLearner object. 
#' 
#' @param out List of \code{CV.SuperLearner} objects
#' @param whichAlgorithm A \code{character} of which algorithm to obtain validation predictions for. 

getPredictionsOnValidation <- function(out, whichAlgorithm){
    Reduce("cbind",lapply(out, FUN=function(s){
        if(whichAlgorithm=="SuperLearner"){
            s$SL.predict
        }else if(whichAlgorithm=="discreteSuperLearner"){
            s$discreteSL.predict
        }else{
            s$library.predict[,which(
                s$libraryNames==whichAlgorithm
            )]
        }
    }))
}
