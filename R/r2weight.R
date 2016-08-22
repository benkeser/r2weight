#' Compute the optimally weight R-squared from CV.SuperLearner objects. 
#' 
#' This function takes a list of CV.SuperLearner objects that were fit using 
#' an identical sample partitioning on different outcomes. The function computes
#' the R-squared for the optimal weighted linear combination of the outcomes and 
#' associated confidence intervals. The function also returns the estimated 
#' R-squared for each outcome individually.
#' 
#' @param cvslList A list of CV.SuperLearner objects on different outcomes 
#' that were executed using an identical sample partitioning with 
#' option saveFitLibrary=TRUE.  \code{cvslList}
#' @param X A list of data.frames that were used in CV.SuperLearner fits. The 
#' length of X should be either one (if same data.frame was used for all 
#' CV.SuperLearner fits or should be equal to length(cvslList). \code{X}
#'
#' @return r2weight A list containing weighted R-squared output
#' @return r2 A list containing single outcome R-squared output
#'
#' @keywords R-squared, cross-validation, prediction
#'
#' @export
#' 
#' @examples
#' library(SuperLearner)
#' library(Rsolnp)
#' X <- data.frame(x1=runif(n=100,0,5), x2=runif(n=100,0,5))
#' Y1 <- rnorm(100, X$x1 + X$x2, 1)
#' Y2 <- rnorm(100, X$x1 + X$x2, 3)
#' 
#' # set seed to ensure identical sample partition
#' set.seed(1234)
#' cvsl1 <- CV.SuperLearner(Y=Y1, X=X, SL.library=c("SL.glm","SL.step"),
#' control=list(saveFitLibrary=TRUE))
#' set.seed(1234)
#' cvsl2 <- CV.SuperLearner(Y=Y2, X=X, SL.library=c("SL.glm","SL.step"),
#' control=list(saveFitLibrary=TRUE))
#' 
#' out <- r2weight(cvslList=list(cvsl1,cvsl2), X=list(X))
#' out

r2weight <- function(
    cvslList, # list of CV.SuperLearner objects
    X # the data frame of predictors used in the CV.SuperLearner
){
    
    #############################################################
    # Data checks
    #############################################################
    # check that all folds are the same
    foldmat <- Reduce("cbind",lapply(cvslList, function(x) unlist(x$folds)))
    correctFolds <- TRUE
    if(length(dim(foldmat)) > 0){
        correctFolds <- all(apply(foldmat, 2, identical, foldmat[,1]))
    }
    if(!correctFolds) stop("not all folds are the same in cvslList")
    
    # check that saveAll and saveFitLibrary is true
    if(is.null(cvslList[[1]]$AllSL)){
        stop("All CV.SuperLearner objects must be run with saveAll=TRUE")
    }else if(!(cvslList[[1]]$AllSL[[1]]$control$saveFitLibrary)){
        stop(paste0("All CV.SuperLearner objects must be run with ", 
                    "control$saveFitLibrary=TRUE"))
    }
    
    # check that X is a list
    if(!is.list(X) | (length(X) != 1 & length(X) != length(cvslList))){ 
        stop("X should be a list of length one or length(cvslList)")
    }
    
    # some parameters used later
    folds <- cvslList[[1]]$folds
    V <- cvslList[[1]]$V
    J <- length(cvslList)
    
    # assemble outcomes
    Ymat <- matrix(Reduce("cbind",lapply(cvslList, function(x) x$Y)),ncol=J)
    n <- length(Ymat[,1])
    
    #############################################################
    # Predictions on training and validation data
    #############################################################
    # list of the predictions from each fold on the training data
    if(length(X) == 1){
        pred <- lapply(split(1:V,1:V), FUN=function(v){
            tmp <- Reduce("cbind",lapply(cvslList, FUN=function(s){
                predict(s$AllSL[[v]], 
                        newdata=X[[1]][-s$folds[[v]],,drop=FALSE])[[1]]
            }))
            tmp
        })
    }else{
        pred <- lapply(split(1:V,1:V), FUN=function(v){
            tmp <- Reduce("cbind",mapply(s=cvslList, x=X, FUN=function(s,x){
                predict(s$AllSL[[v]], 
                        newdata=x[-s$folds[[v]],,drop=FALSE])[[1]]
            }))
            tmp
        })
    }
    
    # list of the predictions from each fold on the validation data
    validPred <- lapply(split(1:V,1:V), FUN=function(v){
        tmp <- Reduce("cbind",lapply(cvslList, FUN=function(s){
            s$SL.predict[folds[[v]]]
        }))
        tmp
    })

    #############################################################
    # Computing R2
    #############################################################
    # get output for each outcome individually
    singleOut <- lapply(split(1:J,1:J),function(j){
        thisWeight <- rep(0,J); thisWeight[j] <- 1
        thisWeightList <- rep(list(thisWeight),V)
        computeR2(Ymat=Ymat, folds=folds, pred=pred, validPred=validPred, 
                  weightList=thisWeightList, V=V, J=J, n=n)
    })
    
    if(length(cvslList)>1){
        # list of weights
        weightList <- lapply(split(1:V,1:V), FUN=function(v){
            Pnv0 <- list(Y = matrix(Ymat[-folds[[v]],],ncol=J),
                         psiHat = matrix(pred[[v]],ncol=J))
            # for each fold estimate weights based on P_{n,v}^0 only
            alphaHat(P = Pnv0)
        })
        
        # get output for weighted R2
        weightedOut <- computeR2(Ymat=Ymat, folds=folds, pred=pred, 
                                 validPred=validPred, 
                                 weightList=weightList, V=V, J=J, n=n)
    }else{
        weightedOut <- singleOut[[1]]
    }
    
  
    out <- list(r2weight=weightedOut, r2=singleOut)
    class(out) <- "r2weight"
    return(out)
}
