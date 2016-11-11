#' print.r2.compare
#' 
#' Print output of optimal weight procedure
#' 

print.r2.compare <- function(object, digits = 3, ...){
    if(object$type=="optWeight"){
        for(j in 1:length(object$Ynames)){
            cat("\n \n R-squared comparison for", object$Ynames[j],": \n")
            if("diff" %in% names(object[[object$Ynames[j]]])){
                diffTable <- sprintf("%0.3f", object[[object$Ynames[j]]]$diff, digits)
                dim(diffTable) <- c(1,4)
                colnames(diffTable) <- c("est","CI.l","CI.h","p")
                row.names(diffTable) <- "diff"
                print(diffTable, quote = FALSE)
            }
            if("ratio" %in% names(object[[object$Ynames[j]]])){
                ratioTable <- sprintf("%0.3f", object[[object$Ynames[j]]]$ratio, digits)
                dim(ratioTable) <- c(1,4)
                colnames(ratioTable) <- c("est","CI.l","CI.h","p")
                row.names(ratioTable) <- "ratio"
                print(ratioTable, quote = FALSE)
            }
        }
    }else{
        cat("\n \nR-squared comparison for optimal weighted outcomes: \n")
        if("diff" %in% names(object)){
            diffTable <- sprintf("%0.3f", object$diff, digits)
            dim(diffTable) <- c(1,4)
            colnames(diffTable) <- c("est","CI.l","CI.h","p")
            row.names(diffTable) <- "diff "
            print(diffTable, quote = FALSE)
        }
        if("ratio" %in% names(object)){
            ratioTable <- sprintf("%0.3f", object$ratio, digits)
            dim(ratioTable) <- c(1,4)
            colnames(ratioTable) <- c("est","CI.l","CI.h","p")
            row.names(ratioTable) <- "ratio"
            print(ratioTable, quote = FALSE)
        }
    }
}
