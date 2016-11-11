#' print.optWeight
#' 
#' Print output of optimal weight procedure
#' 

print.optWeight <- function(object,...){
    cat("\nOptimal weights for prediction with", object$whichAlgorithm, " : \n")
    for(j in 1:length(object$Ynames)){
        cat(object$Ynames[j], " : ", round(object$SL.weights[j],3), "\n")
    }
    cat("\n \nR-squared for each outcome with", object$whichAlgorithm, " : \n")
    r2Table <- round(Reduce("rbind", object$univariateR2),3)
    row.names(r2Table) <- object$Ynames
    colnames(r2Table) <- c("R2","CI.l","CI.h")
    print(r2Table)
}

#' print.r2.optWeight
#' 
#' Print the output of R-squared for Super Learner of optimal R-squared
#' 

print.r2.optWeight <- function(object,...){
    cat("\n \nR-squared for each outcome with", object$whichAlgorithm, " : \n")
    r2Table <- round(Reduce("rbind", object$univariateR2),3)
    row.names(r2Table) <- object$Ynames
    colnames(r2Table) <- c("R2","CI.l","CI.h")
    print(r2Table)
    cat("\n")
    cat("\n \nR-squared for combined outcome with", object$whichAlgorithm, " : \n")
    r2Table2 <- matrix(round(c(object$r2, object$r2.ci),3),nrow=1)
    row.names(r2Table2) <- "weighted combination"
    colnames(r2Table2) <- c("R2","CI.l","CI.h")
    print(r2Table2)
    cat("\n")
}