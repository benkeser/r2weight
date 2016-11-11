#' @method print r2weight    
#' @export

print.r2weight <- function(x,...){
    cat("CV-R2 (95% CI) = ", round(x[[1]]$cv.wR2,3)," (", 
        round(x[[1]]$cv.wR2.ci[1],3)," , ",
        round(x[[1]]$cv.wR2.ci[2],3),")",sep="")
    cat("\n\n")
    cat("Summary across folds: \n ")
    print(x[[1]]$cv.wSummary)
}
