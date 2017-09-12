#' makeData
#' 
#' Makes data for simulation. 
#' 
#' @param n Sample size
#' 
#' @export
#' @importFrom stats runif rbinom rnorm var 

makeData2 <- function(n){
    x1 <- stats::rnorm(n,0,1)
    x2 <- stats::rnorm(n,0,1)
    x3 <- stats::rnorm(n,0,1)
    x4 <- stats::rnorm(n,0,1)
    x5 <- stats::rnorm(n,0,1)
    x6 <- stats::rnorm(n,0,1)
    x7 <- stats::rnorm(n,0,1)
    x8 <- stats::rnorm(n,0,1)
    x9 <- stats::rnorm(n,0,1)
    x10 <- stats::rnorm(n,0,1)
    x11 <- stats::rbinom(n,1,0.5)
    x12 <- stats::rbinom(n,1,0.5)
    x13 <- stats::rbinom(n,1,0.5)
    x14 <- stats::rbinom(n,1,0.5)
    x15 <- stats::rbinom(n,1,0.5)
    x16 <- stats::rbinom(n,1,0.5)
    x17 <- stats::rbinom(n,1,0.5)
    x18 <- stats::rbinom(n,1,0.5)
    x19 <- stats::rbinom(n,1,0.5)
    x20 <- stats::rbinom(n,1,0.5)

    y1 <- stats::rnorm(n,0,1)
    y2 <- stats::rnorm(n,0,1)
    y3 <- stats::rnorm(n,0,1)
    y4 <- stats::rnorm(n,0,1)
    y5 <- stats::rnorm(n,0,1)
    y6 <- stats::rnorm(n,0,1)
    y7 <- stats::rnorm(n,0,1)
    y8 <- stats::rnorm(n,0,1)
    y9 <- stats::rnorm(n,0,1)
    y10 <- stats::rnorm(n,x10^2+x10*x12+x13*x6,1)
    
  return(list(Y=data.frame(y1=y1,y2=y2,y3=y3,y4=y4,y5=y5,y6=y6,y7=y7,y8=y8,y9=y9,y10=y10), 
              X=data.frame(x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,x6=x6,x7=x7,x8=x8,x9=x9,x10=x10,
                           x11=x11,x12=x12,x13=x13,x14=x14,x15=x15,x16=x16,x17=x17,x18=x18,
                           x19=x19,x20=x20)))
}


# Want the simulation to examine power to reject strong null under several alternatives. 
# Consider putting it against the following competitors:
#   - canonical correlation
#   - multivariate linear regression?
#   - linear regression for each outcome + p-value correction/s
#   - PCA reduction of Y's and linear regression
#   - PCA reduction of Y's and machine learning
#   - what is a typical latent variable method?
#
# test against several alternatives
#   - linear relationship in several variables
#   - linear relationship in one variable
#   - non-linear relationship in several variables
#   - non-linear relationship in one variable
#
# 
# probably want to make outcomes correlated with one another so that
# PCA-based methods are more likely to pick up outcomes which are not
# associated with any of the X's











