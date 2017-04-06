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
