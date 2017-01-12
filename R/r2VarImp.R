#' r2VarImp
#' 
#' Compute variable importance for different groups of variables by
#' comparing the R-squared for the optimally combined outcome. 
#' 
#' @param object An \code{r2.optWeight} object. 
#' @param Y The \code{data.frame} of outcomes that was used to fit \code{object}
#' @param X The \code{data.frame} of predictors that was used to fit \code{object}
#' @param verbose A \code{boolean} indicating whether to show a progress bar
#' @param grpX A \code{list} where each entry is a \code{vector} of \code{charater}s 
#' indicating groups of columns of \code{X} to remove to test their importance. Defaults to
#' a list of \code{colnames(X)}. 
#' @param comparison What type of comparison should be made. Possible choices include
#' \code{"diff"} and \code{"ratio"}.   
#' @param parallel A \code{boolean} indicating whether to run the CV.SuperLearner calls
#' in parallel using \code{mclapply}. Be sure to set options()$mc.cores to 
#' @param n.cores A \code{numeric} indicating how many cores to use if \code{parallel = TRUE}. 
#' By default will use \code{detectCores()}. 
#' @param seed The seed to set before each internal call to \code{CV.SuperLearner}
#' @param alpha The function returns a \code{(1-alpha)*100} percent confidence interval. Default
#' is set to \code{0.05} (i.e., 95 percent confidence interval)
#' 
#' @export
#' 
#' @examples 
#' X <- data.frame(x1=runif(n=100,0,5), x2=runif(n=100,0,5))
#' Y1 <- rnorm(100, X$x1 + X$x2, 1)
#' Y2 <- rnorm(100, X$x1 + X$x2, 3)
#' Y <- data.frame(Y1 = Y1, Y2 = Y2)
#' fit <- optWeight(Y = Y, X = X, SL.library = c("SL.glm","SL.mean"), family = gaussian(),outerV = 10, return.CV.SuperLearner = FALSE)
#' perf.fit <- r2.optWeight(object = fit, Y = Y, X = X, evalV = 5)
#' varImp <- r2.varImp(fit, perf.fit, Y = Y, X = X, verbose = TRUE)


r2VarImp <- function(
    optWeightObject, r2.optWeightObject, Y, X, verbose = FALSE, grpX = split(colnames(X),colnames(X)), 
    comparison = c("diff","ratio"), parallel = FALSE, n.cores = detectCores(),
    seed = 12345, alpha = 0.05, ... 
){
    # empty object for r2.optWeight objects 
    xOut <- vector(mode = "list", length = length(grpX))
    for(x in 1:length(grpX)){
        if(verbose){
            cat("Getting importance of c(",grpX[[x]],")\n")
        }
        # call optWeight to get estimated optimal weights
        if(verbose){
            cat("   Fitting initial optWeight\n")
        }
        fit <- optWeight(
            Y = Y, X = X[,-which(colnames(X) %in% grpX[[x]]), drop = FALSE], 
            SL.library = r2.optWeightObject$SL.library,
            CV.SuperLearner.V = r2.optWeightObject$CV.SuperLearner.V, 
            family = r2.optWeightObject$family, whichAlgorithm = r2.optWeightObject$whichAlgorithm,
            parallel = parallel, n.cores = n.cores, seed = seed
            )
        # call r2.optWeight to get performance of estimated optimal weights
        if(verbose){
            cat("   Fitting r2.optWeight\n")
        }
        fit.perf <- r2.optWeight(
            object = fit, Y = Y, X = X[,-which(colnames(X) %in% grpX[[x]]), drop = FALSE], verbose = verbose, 
            evalV = r2.optWeightObject$evalV, seed = seed, parallel = parallel, 
            n.cores = n.cores
        )
        
        # call r2.compare to compare with full fit
        if(verbose){
            cat("   Comparing with full fit\n")
        }
        # each outcome individually
        fit.compare.j <- r2Diff(
            object1 = fit, object2 = optWeightObject, comparison = comparison, alpha = alpha
        )
        # optimal weighted outcome
        fit.compare.opt <- r2Diff(
            object1 = fit.perf, object2 = r2.optWeightObject, comparison = comparison, alpha = alpha
        )
        
        names(xOut)[x] <- paste(grpX[[x]], collapse = "_")
        xOut[[x]] <- list(optWeight = fit, r2.optWeight = fit.perf, 
                          r2Diff = list(univariate = fit.compare.j,
                                            combined = fit.compare.opt))
    }
    
    return(xOut)
}