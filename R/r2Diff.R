#' r2Diff 
#' 
#' Compare the R-squared values in two objects class \code{optWeight} or of 
#' class \code{r2.optWeight}. The former compares the R-squared values
#' for each outcome between the two \code{optWeight} objects, while the latter 
#' compares the R-squared values for the combined outcome of two \code{r2.optWeight}
#' objects. 
#' 
#' @param object1 An object of either class \code{optWeight} or of class 
#' \code{r2.optWeight}. The class type should match that of \code{object2}.
#' @param object2 An object of either class \code{optWeight} or of class 
#' \code{r2.optWeight}. The class type should match that of \code{object1}.
#' @param Y The \code{data.frame} of outcomes that was used to fit \code{object1}
#' and \code{object2}.
#' @param comparison What type of comparison should be made. Possible choices include
#' \code{"diff"} and \code{"ratio"}.
#' @param alpha The function returns a \code{(1-alpha)*100} percent confidence interval. Default
#' is set to \code{0.05} (i.e., 95 percent confidence interval)
#' 
#' @return Point estimate and confidence interval for the selected \code{comparison}. 
#' 
#' @export
#' 
#' @examples 
#' #' @examples
#' X <- data.frame(x1=runif(n=100,0,5), x2=runif(n=100,0,5))
#' Y1 <- rnorm(100, X$x1 + X$x2, 1)
#' Y2 <- rnorm(100, X$x1 + X$x2, 3)
#' Y <- data.frame(Y1 = Y1, Y2 = Y2)
#' fit1 <- optWeight(Y = Y, X = X, SL.library = c("SL.glm","SL.mean"), family = gaussian(), return.CV.SuperLearner = FALSE)
#' perf.fit1 <- r2.optWeight(object = fit1, Y = Y, X = X, evalV = 5)
#' fit2 <- optWeight(Y = Y, X = X[,1,drop=FALSE], SL.library = c("SL.glm","SL.mean"), family = gaussian(),return.CV.SuperLearner = FALSE)
#' perf.fit2 <- r2.optWeight(object = fit2, Y = Y, X = X[,1,drop=FALSE], evalV = 5)
#' 
#' # compare cross-validated r-squared for each outcome
#' comp <- r2.compare(fit1, fit2)
#' comp
#' # compare cross-validated r-squared for combined outcome
#' perf.comp <- r2.compare(perf.fit1, perf.fit2)
#' perf.comp


# TO DO: Add in ability to input one "optWeight" and one "r2.optWeight" object 
# which would then compare the performance for optimally combined outcome to the 
# performance for each outcome individually
r2Diff <- function(
    object1, object2, comparison = c("diff","ratio"), alpha = 0.05
){
    # check classes
    if(class(object1) != class(object2)){
        stop(paste0("object1 and object2 must be of the same class (either both class optWeight",
             "or both class r2.optWeight"))
    }
    
    # check outcomes are same
    if(!all(names(object1$Ynames) == names(object2$Ynames))){
        stop("different outcome names in object1 and object2")
    }
    
    # check that return.IC was set to TRUE
    if(is.null(object1$IC) | is.null(object2$IC)){
        stop("$IC is NULL for either object1 or object2; re-run with return.IC = TRUE")
    }
    
    J <- length(object1$Ynames)
    n <- length(object1$IC$IC.MSE[,1])
    
    # univariate outcome comparisons
    if(class(object1) == "optWeight"){
        # initialize out
        out <- vector(mode = "list", length = J)
        names(out) <- object1$Ynames
        # loop over each outcome
        for(j in 1:J){
            out[[j]] <- vector(mode = "list")
            out$diff <- NULL; out$ratio <- NULL
            # matrix of influence functions
            ICMat.j <- cbind(
                object1$IC$IC.MSE[,j],
                object1$IC$IC.Var[,j],
                object2$IC$IC.MSE[,j],
                object2$IC$IC.Var[,j]
            )
            # mse's and variances used to compute gradient
            psi.j <- c(object1$MSE[j],object1$Var[j],
                       object2$MSE[j],object2$Var[j])
            
            # get se of difference
            se.diff.j <- NULL
            if("diff" %in% comparison){
                g.diff.j <- matrix(c(-1/psi.j[2], psi.j[1]/(psi.j[2]^2),
                            1/psi.j[4], -psi.j[3]/(psi.j[4]^2)),nrow=4)
                se.diff.j <- sqrt(t(g.diff.j)%*%crossprod(ICMat.j)%*%g.diff.j)/n
                diff.j <- (1 - psi.j[1]/psi.j[2]) - (1 - psi.j[3]/psi.j[4])
                # put results in out
                out[[j]]$diff <- data.frame(
                    # point estimate
                    est = diff.j,
                    # lower ci
                    CI.l = diff.j - qnorm(1-alpha/2)*se.diff.j,
                    # upper ci
                    CI.h = diff.j + qnorm(1-alpha/2)*se.diff.j,
                    # p-value for wald test that point.j = 0
                    p = 2*pnorm(-abs(diff.j/se.diff.j))
                )
            }
            # get se of ratio
            se.ratio.j <- NULL
            if("ratio" %in% comparison){
                g.log.ratio.j <- matrix(c(1/psi.j[1], -1/psi.j[2],
                                      -1/psi.j[3], 1/psi.j[4]),nrow=4)
                se.log.ratio.j <- sqrt(t(g.log.ratio.j)%*%crossprod(ICMat.j)%*%g.log.ratio.j)/n
                log.ratio.j <- log(psi.j[1]/psi.j[2] / (psi.j[3]/psi.j[4]))
                # put results in out
                out[[j]]$ratio <- data.frame(
                    # point estimate
                    est = exp(log.ratio.j),
                    # lower ci
                    CI.l = exp(log.ratio.j - qnorm(1-alpha/2)*se.log.ratio.j),
                    # upper ci
                    CI.h = exp(log.ratio.j + qnorm(1-alpha/2)*se.log.ratio.j), 
                    # p-value for wald test that point.j = 0
                    p = 2*pnorm(-abs(log.ratio.j/se.log.ratio.j))
                )
            }
        }
    # comparisons of weighted outcomes
    }else if(class(object1) == "r2.optWeight"){
        # initialize out
        out <- vector(mode = "list")
        out$diff <- NULL; out$ratio <- NULL
        # matrix of influence functions
        ICMat <- cbind(
            object1$IC$IC.MSE,
            object1$IC$IC.Var,
            object2$IC$IC.MSE,
            object2$IC$IC.Var
        )
        # mse's and variances used to compute gradient
        psi <- c(object1$MSE,object1$Var,
                 object2$MSE,object2$Var)
            
        # get se of difference
        se.diff <- NULL
        if("diff" %in% comparison){
            g.diff <- matrix(c(-1/psi[2], psi[1]/(psi[2]^2),
                               1/psi[4], -psi[3]/(psi[4]^2)),nrow=4)
            se.diff <- sqrt(t(g.diff)%*%crossprod(ICMat)%*%g.diff)/n
            diff <- (1-psi[1]/psi[2]) - (1-psi[3]/psi[4])
            # put results in out
            out$diff <- data.frame(
            # point estimate
                est = diff,
                # lower ci
                CI.l = diff - qnorm(1-alpha/2)*se.diff,
                # upper ci
                CI.h = diff + qnorm(1-alpha/2)*se.diff,
                # p-value for wald test that point = 0
                p = 2*pnorm(-abs(diff/se.diff))
            )
        }
        # get se of ratio
        se.ratio <- NULL
        if("ratio" %in% comparison){
            g.log.ratio <- matrix(c(1/psi[1], -1/psi[2],
                                    -1/psi[3], 1/psi[4]),nrow=4)
            se.log.ratio <- sqrt(t(g.log.ratio)%*%crossprod(ICMat)%*%g.log.ratio)/n
            log.ratio <- log(psi[1]/psi[2] / (psi[3]/psi[4]))
            # put results in out
            out$ratio <- data.frame(
                # point estimate
                est = exp(log.ratio),
                # lower ci
                CI.l = exp(log.ratio - qnorm(1-alpha/2)*se.log.ratio),
                # upper ci
                CI.h = exp(log.ratio + qnorm(1-alpha/2)*se.log.ratio), 
                # p-value for wald test that point = 0
                p = 2*pnorm(-abs(log.ratio/se.log.ratio))
            )
        }
    }else{
        stop(paste0("object1 and object2 must be either of class 'optWeight'",
                    "or class 'r2.optWeight'")) 
    }
    out$type <- ifelse(class(object1) == "optWeight", "optWeight", "r2.optWeight")
    out$Ynames <- object1$Ynames
    class(out) <- "r2.compare"
    return(out)
}
















