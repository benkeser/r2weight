#' summary.r2_varImp
#' 
#' Method for summarizing output of r2.varImport.
#' 
#' @param object An object of class \code{r2.varImport}
#' @param ... Other arguments
#' @export

summary.r2_varImp <- function(
    object, ...
){
    return(NULL)
}

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

#' print.optWeight
#' 
#' Print output of optimal weight procedure
#' 
#' @param x An object of class \code{optWeight}
#' @param ... Other args (not used)
#' @export 

print.optWeight <- function(x,...){
    cat("\nOptimal weights for prediction with", x$whichAlgorithm, " : \n")
    J <- length(x$Ynames)
    for(j in 1:J){
        cat(x$Ynames[j], " : ", round(x$SL.weights[j],3), "\n")
    }
    cat("\n \nR-squared for each outcome with", x$whichAlgorithm, " : \n")
    r2Table <- round(Reduce("rbind", x$univariateR2),3)
    dim(r2Table) <- c(J, 4)
    row.names(r2Table) <- x$Ynames
    colnames(r2Table) <- c("R2","CI.l","CI.h","pval")
    print(r2Table)
}

#' print.r2_optWeight
#' 
#' Print the output of R-squared for Super Learner of optimal R-squared
#' 
#' @param x An object of class r2_optWeight
#' @param ... Other args (not used)
#' @export

print.r2_optWeight <- function(x,...){
    cat("\n \nR-squared for each outcome with", x$whichAlgorithm, " : \n")
    r2Table <- round(Reduce("rbind", x$univariateR2),3)
    row.names(r2Table) <- x$Ynames
    colnames(r2Table) <- c("R2","CI.l","CI.h","pval")
    print(r2Table)
    cat("\n")
    cat("\n \nR-squared for combined outcome with", x$whichAlgorithm, " : \n")
    r2Table2 <- matrix(round(c(x$r2, x$r2.ci, x$r2.pval),3),nrow=1)
    row.names(r2Table2) <- "weighted combination"
    colnames(r2Table2) <- c("R2","CI.l","CI.h","pval")
    print(r2Table2)
    cat("\n")
}


#' print.r2_diff
#' 
#' Print output of optimal weight procedure
#' 
#' @param x An object of class \code{r2_diff}
#' @param ... Other arguments (passed to \code{round})
#' 
#' @export

print.r2_diff <- function(x, ...){
    if(x$type=="optWeight"){
        for(j in 1:length(x$Ynames)){
            cat("\n \n R-squared comparison for", x$Ynames[j],": \n")
            if("diff" %in% names(x[[x$Ynames[j]]])){
                diffTable <- sprintf("%0.3f", x[[x$Ynames[j]]]$diff, ...)
                dim(diffTable) <- c(1,4)
                colnames(diffTable) <- c("est","CI.l","CI.h","p")
                row.names(diffTable) <- "diff"
                print(diffTable, quote = FALSE)
            }
            if("ratio" %in% names(x[[x$Ynames[j]]])){
                ratioTable <- sprintf("%0.3f", x[[x$Ynames[j]]]$ratio, ...)
                dim(ratioTable) <- c(1,4)
                colnames(ratioTable) <- c("est","CI.l","CI.h","p")
                row.names(ratioTable) <- "ratio"
                print(ratioTable, quote = FALSE)
            }
        }
    }else{
        cat("\n \nR-squared comparison for optimal weighted outcomes: \n")
        if("diff" %in% names(x)){
            diffTable <- sprintf("%0.3f", x$diff, ...)
            dim(diffTable) <- c(1,4)
            colnames(diffTable) <- c("est","CI.l","CI.h","p")
            row.names(diffTable) <- "diff "
            print(diffTable, quote = FALSE)
        }
        if("ratio" %in% names(x)){
            ratioTable <- sprintf("%0.3f", x$ratio, ...)
            dim(ratioTable) <- c(1,4)
            colnames(ratioTable) <- c("est","CI.l","CI.h","p")
            row.names(ratioTable) <- "ratio"
            print(ratioTable, quote = FALSE)
        }
    }
}
