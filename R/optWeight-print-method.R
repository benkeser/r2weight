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
    dim(r2Table) <- c(J, 3)
    row.names(r2Table) <- x$Ynames
    colnames(r2Table) <- c("R2","CI.l","CI.h")
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
    colnames(r2Table2) <- c("R2","CI.l","CI.h")
    print(r2Table2)
    cat("\n")
}