#' r2_varImp
#' 
#' Compute variable importance for different groups of variables by
#' comparing the R-squared for the optimally combined outcome. 
#' 
#' @param optWeightObject An \code{optWeight} object
#' @param r2_optWeightObject An \code{r2_optWeight} object
#' @param Y The \code{data.frame} of outcomes that was used to fit \code{object}
#' @param X The \code{data.frame} of predictors that was used to fit \code{object}
#' @param verbose A \code{boolean} indicating whether to show a progress bar
#' @param grpX A \code{list} where each entry is a \code{vector} of \code{charater}s 
#' indicating groups of columns of \code{X} to remove to test their importance. Defaults to
#' a list of \code{colnames(X)}
#' @param comparison What type of comparison should be made. Possible choices include
#' \code{"diff"} and \code{"ratio"}   
#' @param parallel A \code{boolean} indicating whether to run the CV.SuperLearner calls
#' in parallel using \code{mclapply}. Be sure to set options()$mc.cores to 
#' @param n.cores A \code{numeric} indicating how many cores to use if \code{parallel = TRUE}. 
#' By default will use \code{parallel::detectCores()}
#' @param seed The seed to set before each internal call to \code{CV.SuperLearner}
#' @param alpha The function returns a \code{(1-alpha)*100} percent confidence interval. Default
#' is set to \code{0.05} (i.e., 95 percent confidence interval)
#' @param ... Other arguments (not currently used)
#' 
#' @export
#' 
#' @importFrom parallel detectCores
#' 
#' @examples 
#' X <- data.frame(x1=runif(n=100,0,5), x2=runif(n=100,0,5))
#' Y1 <- rnorm(100, X$x1 + X$x2, 1)
#' Y2 <- rnorm(100, X$x1 + X$x2, 3)
#' Y <- data.frame(Y1 = Y1, Y2 = Y2)
#' fit <- optWeight(Y = Y, X = X, SL.library = c("SL.glm","SL.mean"), 
#' family = "gaussian",outerV = 10, return.CV.SuperLearner = FALSE)
#' perf.fit <- r2_optWeight(object = fit, Y = Y, X = X, evalV = 5)
#' varImp <- r2_varImp(fit, perf.fit, Y = Y, X = X)


r2_varImp <- function(
    optWeightObject, r2_optWeightObject, Y, X, verbose = FALSE, grpX = split(colnames(X),colnames(X)), 
    comparison = c("diff","ratio"), parallel = FALSE, n.cores = parallel::detectCores(),
    seed = 12345, alpha = 0.05, ... 
){
    # empty object for r2_optWeight objects 
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
            SL.library = r2_optWeightObject$SL.library,
            CV.SuperLearner.V = r2_optWeightObject$CV.SuperLearner.V, 
            family = r2_optWeightObject$family, whichAlgorithm = r2_optWeightObject$whichAlgorithm,
            parallel = parallel, n.cores = n.cores, seed = seed
            )
        # call r2_optWeight to get performance of estimated optimal weights
        if(verbose){
            cat("   Fitting r2_optWeight\n")
        }
        fit.perf <- r2_optWeight(
            object = fit, Y = Y, X = X[,-which(colnames(X) %in% grpX[[x]]), drop = FALSE], verbose = verbose, 
            evalV = r2_optWeightObject$evalV, seed = seed, parallel = parallel, 
            n.cores = n.cores
        )
        
        # call r2_diff to compare with full fit
        if(verbose){
            cat("   Comparing with full fit\n")
        }
        # each outcome individually
        fit.compare.j <- r2_diff(
            object1 = fit, object2 = optWeightObject, comparison = comparison, alpha = alpha
        )
        # optimal weighted outcome
        fit.compare.opt <- r2_diff(
            object1 = fit.perf, object2 = r2_optWeightObject, comparison = comparison, alpha = alpha
        )
        
        names(xOut)[x] <- paste(grpX[[x]], collapse = "_")
        xOut[[x]] <- list(optWeight = fit, r2_optWeight = fit.perf, 
                          r2_diff = list(univariate = fit.compare.j,
                                            combined = fit.compare.opt))
    }
    
    return(xOut)
}