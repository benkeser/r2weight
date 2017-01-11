#' r2
#' 
#' S3 method for object of class \code{optWeight}.
#' @exportMethod 
r2 <- function(x) UseMethod("r2")

#' r2.optWeight
#' 
#' Estimate the performance of the Super Learner for predicting the optimal weighted 
#' combination via cross-validation.
#' 
#' @param object A \code{optWeight} object. 
#' @param Y The \code{data.frame} of outcomes that was used to fit \code{object}
#' @param X The \code{data.frame} of predictors that was used to fit \code{object}
#' @param evalV The number of outer cross validation folds to use to evaluate the predictive 
#' performance of \code{object} for predicting the optimal combined outcome. 
#' @param return.IC A \code{boolean} indicating whether to return estimated influence
#' function at the observed data values (needed for post-hoc comparisons).
#' @param verbose A \code{boolean} indicating whether to show a progress bar
#' @param parallel A \code{boolean} indicating whether to run the CV.SuperLearner calls
#' in parallel using \code{mclapply}. Be sure to set options()$mc.cores to 
#' @param n.cores A \code{numeric} indicating how many cores to use if \code{parallel = TRUE}. 
#' By default will use \code{detectCores()}. 
#' @return An cross-validated estimate of the R-squared for the optimal prediction and 
#' standard error and confidence interval. 
#' 
#' @examples
#' # Example 1 -- simple fit 
#' X <- data.frame(x1=runif(n=100,0,5), x2=runif(n=100,0,5))
#' Y1 <- rnorm(100, X$x1 + X$x2, 1)
#' Y2 <- rnorm(100, X$x1 + X$x2, 3)
#' Y <- data.frame(Y1 = Y1, Y2 = Y2)
#' fit <- optWeight(Y = Y, X = X, SL.library = c("SL.glm","SL.mean"), family = gaussian(),outerV = 10, return.CV.SuperLearner = FALSE)
#' perf.fit <- r2.optWeight(object = fit, Y = Y, X = X, evalV = 5)
#' 
#' 
#' @export


r2.optWeight <- function(
    object, Y, X, evalV = 10, return.IC = TRUE, 
    seed = 12345, verbose = FALSE, 
    parallel = FALSE, n.cores = detectCores(), ...
){
    n <- length(Y[,1])
    validRows <- split(sample(1:n), rep(1:evalV, length = n))
    
    # cross-validate
    if(verbose){
        ct <- 0
        pb <- txtProgressBar(style=3)
        env <- environment()
    }
    CV.rslt <- Reduce("rbind",lapply(validRows, FUN = function(v){
        if(verbose){
            assign("ct", ct+1, envir = env)
        }
        tmp <- .doOneEval(validRows = v, X = X, Y = Y, object = object, seed = seed,
                   return.IC = return.IC, parallel = parallel, n.cores = n.cores)
        if(verbose) eval.parent(setTxtProgressBar(pb, ct/evalV))
        return(tmp)
    }))
    
    # close progress bar
    if(verbose) close(pb)
    
    # get R^2 results
    ord <- order(unlist(validRows))
    R2.rslt <- getUnivariateR2(Y = matrix(CV.rslt[ord,1]), 
                               psiHat.Pnv0 = matrix(CV.rslt[ord,2]),
                               return.IC = return.IC)
    
    # format output 
    out <- vector(mode = "list")
    out$Ynames <- colnames(Y)
    out$r2 <- R2.rslt[[1]][1]
    out$r2.ci <- R2.rslt[[1]][2:3]
    out$MSE <- R2.rslt$MSE
    out$Var <- R2.rslt$Var
    out$IC <- NULL
    out$evalV <- evalV
    out$whichAlgorithm <- object$whichAlgorithm
    out$SL.library <- object$SL.library
    out$CV.SuperLearner.V <- object$CV.SuperLearner.V
    out$family <- object$family
    if(return.IC){
        out$IC <- R2.rslt$IC
    }
    out$univariateR2 <- object$univariateR2[colnames(Y)]
    class(out) <- "r2.optWeight"
    return(out)
}