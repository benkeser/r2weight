#' makeData1
#' 
#' Makes data for simulation. 
#' @export

makeData <- function(n){
    x1 <- runif(n,0,4)
    x2 <- runif(n,0,4)
    x3 <- runif(n,0,4)
    x4 <- rbinom(n,1,0.75)
    x5 <- rbinom(n,1,0.25)
    x6 <- rbinom(n,1,0.5)
    
    x7 <- runif(n,0,4)
    x8 <- runif(n,0,4)
    x9 <- runif(n,0,4)
    
    commonMean <- x1/0.25 + x2/0.25 + x3/0.25 + x4 + x5 + x6
    
    # first outcome
    err1 <- rnorm(n, 0, 5)
    y1 <- commonMean + err1 + x7/0.25
    
    # second outcome
    err2 <- rnorm(n, 0, 5)
    y2 <- commonMean + err2 + x8/0.25
    
    # third outcome
    err3 <- rnorm(n, 0, 5)
    y3 <- commonMean + err3 + x9/0.25
    
  return(list(Y=data.frame(y1=y1,y2=y2,y3=y3), 
              X=data.frame(x1=x1,x2=x2,x3=x3,x4=x4,x5=x5)))
}

#' getTrueWeights1
#' 
#' Compute the true value of the optimal weights for simulation 1.
#' @export

getTrueWeights <- function(n = 1e6){
    x1 <- runif(n,0,4)
    x2 <- runif(n,0,4)
    x3 <- runif(n,0,4)
    x4 <- rbinom(n,1,0.75)
    x5 <- rbinom(n,1,0.25)
    x6 <- rbinom(n,1,0.5)
    x7 <- runif(n,0,4)
    x8 <- runif(n,0,4)
    x9 <- runif(n,0,4)
    
    commonMean <- x1 + x2/0.5 + x3/0.25 + x4 + x5/0.5 + x6/0.25
    
    # first outcome
    err1 <- rnorm(n, 0, 5)
    y1 <- commonMean + err1 + x7/0.5
    
    # second outcome
    err2 <- rnorm(n, 0, 5)
    y2 <- commonMean + err2 + x8/0.5
    
    # third outcome
    err3 <- rnorm(n, 0, 5)
    y3 <- commonMean + err3 + x9/0.5
    
    psi <- cbind(
        commonMean + x7/0.5,
        commonMean + x8/0.5,
        commonMean + x9/0.5
    )
    alphaHat(Y = cbind(y1,y2,y3),
             psiHat.Pnv0 = psi)
} 

#' getTrueOptR2
#' 
#' Compute the true value of R^2_{0,omega_0}
#' @export

getTrueOptR2 <- function(n=1e6){
    x1 <- runif(n,0,4)
    x2 <- runif(n,0,4)
    x3 <- runif(n,0,4)
    x4 <- rbinom(n,1,0.75)
    x5 <- rbinom(n,1,0.25)
    x6 <- rbinom(n,1,0.5)
    x7 <- runif(n,0,4)
    x8 <- runif(n,0,4)
    x9 <- runif(n,0,4)
    
    commonMean <- x1 + x2/0.5 + x3/0.25 + x4 + x5/0.5 + x6/0.25
    
    # first outcome
    err1 <- rnorm(n, 0, 5)
    y1 <- commonMean + err1 + x7/0.5
    
    # second outcome
    err2 <- rnorm(n, 0, 5)
    y2 <- commonMean + err2 + x8/0.5
    
    # third outcome
    err3 <- rnorm(n, 0, 5)
    y3 <- commonMean + err3 + x9/0.5
    
    # combining outcomes and predictions
    omega0 <- matrix(rep(1/3,3))
    y <- cbind(y1,y2,y3)
    psi <- cbind(
        commonMean + x7/0.5,
        commonMean + x8/0.5,
        commonMean + x9/0.5
    )
    y_omega0 <- y%*%omega0
    psi_omega0 <- psi%*%omega0
    
    mse_omega0 <- mean((y_omega0 - psi_omega0)^2)
    var_omega0 <- var(y_omega0)
    
    return(
        1 - mse_omega0/var_omega0
    )
}


#' getTrueUnivariateR2
#' 
#' Compute the true value of R^2_{0,j}. The code computes it only for 
#' y1 (from \code{makeData}), but it will be the same for all outcomes
#' @export

getTrueUnivarateR2 <- function(n=1e6){
    x1 <- runif(n,0,4)
    x2 <- runif(n,0,4)
    x3 <- runif(n,0,4)
    x4 <- rbinom(n,1,0.75)
    x5 <- rbinom(n,1,0.25)
    x6 <- rbinom(n,1,0.5)
    x7 <- runif(n,0,4)

    commonMean <- x1 + x2/0.5 + x3/0.25 + x4 + x5/0.5 + x6/0.25
    
    # first outcome
    err1 <- rnorm(n, 0, 5)
    y1 <- commonMean + err1 + x7/0.5
    psi <- commonMean + x7/0.5
    mse <- mean((y1-psi)^2)
    return(
        1 - mse/var(y1)
    )
}
