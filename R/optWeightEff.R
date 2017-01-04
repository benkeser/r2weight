#' optWeightEff
#' 
#' Function that computes the optimal combination of multiple outcomes and a predictor of
#' the optimal combination using Super Learning. 
#' 
#' @param Y A \code{data.frame} of outcomes with each column representing a different outcome
#' @param X A \code{data.frame} that will be used to predict each outcome. 
#' @param Z A \code{vector} of binary treatment assignments (= 0 or 1)
#' @param SL.library A \code{vector} or \code{list} of the Super Learner library to be used for the
#' prediction. See \code{?SuperLearner} for more details. For now the same \code{SL.library} is used
#' for prediction of each outcome. 
#' @param V The number of cross validation folds to use to define parameter. 
#' @param family An object of class \code{family} equal to either \code{gaussian()} for continuous
#' outcomes or \code{binomial()} for binary outcomes. 
#' @param seed The seed to set before each internal call to \code{CV.SuperLearner}
#' @param select A \code{character} indicating whether or not to select weights based on 
#' the estimated p-value of the effect (\code{"pvalue"}) or the absolute value of the estimated
#' effect size (\code{"effect"}). 
#' @param CV.SuperLearner.V The number of CV folds for the calls to \code{CV.SuperLearner}. For now, the inner
#' calls to \code{CV.SuperLearner} always use V=10. 
#' @param return.SuperLearner A \code{boolean} indicating whether to return the fitted \code{SuperLearner}
#' objects for each outcome. Default is \code{TRUE}, as these fits are needed for later predictions. 
#' @param parallel Not developed yet
#' @param n.cores A \code{numeric} indicating how many cores to use if \code{parallel = TRUE}. 
#' By default will use \code{detectCores()}. Not developed yet.
#' @param return.IC A \code{boolean} indicating whether to return vectors of cross validated 
#' influence curve estimates at the observed data.
#' @param ... Other arguments
#' 
#' @return TO DO: Add return documentation. 
#' 
#' @export
#' 
#' @importFrom SuperLearner CV.SuperLearner SuperLearner
#' @examples
#' # Example 1 -- simple fit
#' X <- data.frame(x1=runif(n=500,0,5), x2=runif(n=500,0,5))
#' Z <- rbinom(500, 1, plogis(-2 + X$x1 + X$x2))
#' Y1 <- rnorm(500, X$x1 + X$x2 + 1*Z, 1)
#' Y2 <- rnorm(500, X$x1 + X$x2 + 0.25*Z, 1)
#' Y <- data.frame(Y1 = Y1, Y2 = Y2)
#' fit <- optWeightEff(Y = Y, X = X, Z=Z, SL.library.Q = c("SL.glm","SL.mean"),
#'                      SL.library.g = c("SL.glm","SL.mean"))


optWeightEff <- function(Y, X, SL.library.Q, SL.library.g,
                         V = 10, return.SuperLearner = FALSE,
                         select = "pvalue",
                         SuperLearner.V = 10, 
                         return.IC = TRUE,
                         parallel = FALSE,
                         family = gaussian(),
                         n.cores = detectCores(),
                         ...){
    #--------------------
    # Workflow 
    #--------------------
    # 1. Fit do with same seeds for each outcome
    # 2. Compute estimate of alpha_n
    # 3. Fit SL on all the data
    # 4. Return fit for all ncol(Y) SL's + weights
    # 
    # write a predict.optWeight function that can be used to generate new 
    # predictions of the optimal weighted outcome. 
    
    # get some initial parameter values
    n <- length(Y[,1])
    J <- ncol(Y)
    validRows <- split(sample(1:n), rep(1:V, length = n))
    
    # fit doOneEvalEff in each fold
    CV.rslt <- lapply(validRows, FUN = function(v){
        tmp <- .doOneEvalEff(validRows = v, X = X, Y = Y, Z = Z, seed = seed,
                             SuperLearner.V = SuperLearner.V,
                             SL.library.Q = SL.library.Q, SL.library.g = SL.library.g,
                             select = select, family = family)
        return(tmp)
    })
    
    # unlist all results
    ord <- order(unlist(validRows))
    Ycombn <- unlist(lapply(CV.rslt, "[[", 1))[ord]
    Qn1 <- unlist(lapply(CV.rslt,"[[",2))[ord]
    Qn0 <- unlist(lapply(CV.rslt,"[[",3))[ord]
    gn <- unlist(lapply(CV.rslt,"[[",4))[ord]
    alphaList <- lapply(CV.rslt,"[[",5)
    
    # now do targeting with these estimates
    cv.tmle.fit <- tmle(Y = Ycombn, W = X, A = Z, Q = cbind(Qn0, Qn1), g1W = gn)
    
    #---------------------------------
    # format the output
    #---------------------------------
    out <- vector(mode = "list")
    
    # TO DO: I don't like some of these names for the output.
    #        Think of better names. 
    out$SL.fits <- NULL
    if(return.SuperLearner){
        out$SL.fits <- SuperLearner.list
    }
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