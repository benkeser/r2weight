#' optWeight
#' 
#' Function that computes the optimal combination of multiple outcomes and a predictor of
#' the optimal combination using Super Learning. 
#' 
#' @param Y A \code{data.frame} of outcomes with each column representing a different outcome
#' @param X A \code{data.frame} that will be used to predict each outcome. 
#' @param SL.library A \code{vector} or \code{list} of the Super Learner library to be used for the
#' prediction. See \code{?SuperLearner} for more details. For now the same \code{SL.library} is used
#' for prediction of each outcome. 
#' @param family An object of class \code{family} equal to either \code{gaussian()} for continuous
#' outcomes or \code{binomial()} for binary outcomes. 
#' @param seed The seed to set before each internal call to \code{CV.SuperLearner}
#' @param CV.SuperLearner.V The number of CV folds for the calls to \code{CV.SuperLearner}. For now, the inner
#' calls to \code{CV.SuperLearner} always use V=10. 
#' @param whichAlgorithm What algorithm to compute optimal predictions and R^2 values for.
#' @param return.CV.SuperLearner A \code{boolean} indicating whether to return the fitted CV.SuperLearner
#' objects. 
#' @param parallel A \code{boolean} indicating whether to run the CV.SuperLearner calls
#' in parallel using \code{mclapply}. Be sure to set options()$mc.cores to 
#' @param n.cores A \code{numeric} indicating how many cores to use if \code{parallel = TRUE}. 
#' By default will use \code{detectCores()}. 
#' @param ... Other arguments
#' 
#' @return TO DO: Add return documentation. 
#' 
#' @export
#' 
#' @importFrom SuperLearner CV.SuperLearner SuperLearner
#' @examples
#' # Example 1 -- simple fit
#' X <- data.frame(x1=runif(n=100,0,5), x2=runif(n=100,0,5))
#' Y1 <- rnorm(100, X$x1 + X$x2, 1)
#' Y2 <- rnorm(100, X$x1 + X$x2, 3)
#' Y <- data.frame(Y1 = Y1, Y2 = Y2)
#' system.time(
#'  fit <- optWeight(Y = Y, X = X, SL.library = c("SL.glm","SL.mean","SL.randomForest"))
#' )
#' # Example 2 -- simple fit with parallelization
#' system.time(
#'    fit <- optWeight(Y = Y, X = X, SL.library = c("SL.glm","SL.mean","SL.randomForest"), parallel = TRUE, n.cores = 3)
#' )
#' 
#' 

optWeight <- function(Y, X, SL.library, family = gaussian(), CV.SuperLearner.V = 10, 
                      seed = 12345, 
                      whichAlgorithm = "SuperLearner", 
                      return.CV.SuperLearner = FALSE,
                      return.IC = TRUE,
                      parallel = FALSE,
                      n.cores = detectCores(),
                      ...){
    #--------------------
    # Workflow 
    #--------------------
    # 1. Fit CV.SuperLearner with same seeds for each outcome
    # 2. Compute estimate of alpha_n
    # 3. Fit SL on all the data
    # 4. Return fit for all ncol(Y) SL's + weights
    # 
    # write a predict.optWeight function that can be used to generate new 
    # predictions of the optimal weighted outcome. 
    
    # get some initial parameter values
    n <- length(Y[,1])
    J <- ncol(Y)
    Ymat <- data.matrix(Y)
    
    # fit CV.SuperLearner
    CV.SuperLearner.list <- apply(Ymat, 2, function(y){
        set.seed(seed)
        if(parallel){
            options("mc.cores" = n.cores)
        }
        fit <- SuperLearner::CV.SuperLearner(
            Y = y, X = X, SL.library = SL.library, family = family, V = CV.SuperLearner.V,
            parallel = ifelse(parallel, "multicore","seq")
        )
    })
    
    # get predictions back for each outcome for the specified algorithm
    psiHat.Pnv0 <- getPredictionsOnValidation(out = CV.SuperLearner.list, 
                                              whichAlgorithm = whichAlgorithm)
    
    #---------------------------------
    # compute R^2 for each outcome
    #---------------------------------
    univariateResults <- getUnivariateR2(Y = Ymat, psiHat.Pnv0 = psiHat.Pnv0,
                                         return.IC = return.IC)
    
    #---------------------------------
    # Estimate the optimal weight
    #---------------------------------
    alpha_n <- alphaHat(Y = Ymat, psiHat.Pnv0 = psiHat.Pnv0)
    
    # fit Super Learner to full data
    SuperLearner.list <- apply(Ymat, 2, function(y){
        set.seed(seed)
        fit <- SuperLearner::SuperLearner(
            Y = y, X = X, SL.library = SL.library, family = family
        )
    })
    
    #---------------------------------
    # format the output
    #---------------------------------
    out <- vector(mode = "list")
    
    # TO DO: I don't like some of these names for the output.
    #        Think of better names. 
    out$SL.fits <- SuperLearner.list
    out$SL.weights <- alpha_n
    out$SL.library <- SL.library
    out$CV.SL.fits <- NULL
    out$whichAlgorithm <- whichAlgorithm 
    out$CV.SuperLearner.V <- CV.SuperLearner.V
    out$family <- family
    if(return.CV.SuperLearner){
        out$CV.SL.fits <- CV.SuperLearner.list
    }
    out$univariateR2 <- univariateResults[colnames(Y)]
    out$IC <- NULL
    if(return.IC){
        out$IC <- univariateResults[["IC"]]
    }
    out$MSE <- univariateResults[["MSE"]]
    out$Var <- univariateResults[["Var"]]
    out$Ynames <- colnames(Y)
    class(out) <- "optWeight"
    return(out)
}