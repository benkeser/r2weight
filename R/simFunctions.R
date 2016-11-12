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
    
  return(list(Y=data.frame(y1=y1,y2=y2,y3=y3), 
              X=data.frame(x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,x6=x6,x7=x7,x8=x8,x9=x9)))
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

getTrueUnivariateR2 <- function(n=1e6){
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



#' getTrueUnivariateR2Exclude
#' 
#' Compute the true value of R^2_{0,j} when a given variable of X is excluded. 
#' @export
#' 

getTrueUnivariateR2Exclude <- function(n=1e6, excludeX, whichY){
    if(excludeX < 0 | excludeX > 9){
        stop("excludeX between 1-9")
    }
    if(whichY < 0 | whichY > 3){
        stop("whichY between 1-3")
    }
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
    
    # exclude X
    # replace each x by its mean
    commonPsi <- if(excludeX==1){
        2 + x2/0.5 + x3/0.25 + x4 + x5/0.5 + x6/0.25
    }else if(excludeX==2){
        x1 + 2/0.5 + x3/0.25 + x4 + x5/0.5 + x6/0.25
    }else if(excludeX==3){
        x1 + x2/0.5 + 2/0.25 + x4 + x5/0.5 + x6/0.25
    }else if(excludeX==4){
        x1 + x2/0.5 + x3/0.25 + 0.75 + x5/0.5 + x6/0.25
    }else if(excludeX==5){
        x1 + x2/0.5 + x3/0.25 + x4 + 0.25/0.5 + x6/0.25
    }else if(excludeX==6){
        x1 + x2/0.5 + x3/0.25 + x4 + x5/0.5 + 0.5/0.25    
    }else{ # if exclude is 7,8,9
        commonMean
    }
    
    # extra term for either x7,8,or 9
    extraMean <- if(whichY==1){
        x7/0.5
    }else if(whichY==2){
        x8/0.5
    }else if(whichY==3){
        x9/0.5
    }
    
    # outcome
    err <- rnorm(n, 0, 5)
    y <- commonMean + err + extraMean
    
    extraPsi <- extraMean
    if((excludeX == 7 & whichY==1) |
       (excludeX == 8 & whichY==2) |
       (excludeX == 9 & whichY==3)){
        extraPsi <- 2/0.5
    }

    # predictor
    psi <- commonPsi + extraPsi
    # mse
    mse <- mean((y-psi)^2)
    return(
        1 - mse/var(y)
    )
}



#' getTrueOptR2Exclude
#' 
#' Compute the true value of R^2_{0,omega_0} when a given variable of X is excluded. 
#' @export
#' 

getTrueOptR2Exclude <- function(n=1e6, excludeX){
    if(excludeX < 0 | excludeX > 9){
        stop("excludeX between 1-9")
    }

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
    
    # exclude X
    # replace each x by its mean
    commonPsi <- if(excludeX==1){
        2 + x2/0.5 + x3/0.25 + x4 + x5/0.5 + x6/0.25
    }else if(excludeX==2){
        x1 + 2/0.5 + x3/0.25 + x4 + x5/0.5 + x6/0.25
    }else if(excludeX==3){
        x1 + x2/0.5 + 2/0.25 + x4 + x5/0.5 + x6/0.25
    }else if(excludeX==4){
        x1 + x2/0.5 + x3/0.25 + 0.75 + x5/0.5 + x6/0.25
    }else if(excludeX==5){
        x1 + x2/0.5 + x3/0.25 + x4 + 0.25/0.5 + x6/0.25
    }else if(excludeX==6){
        x1 + x2/0.5 + x3/0.25 + x4 + x5/0.5 + 0.5/0.25    
    }else{ # if exclude is 7,8,9
        commonMean
    }
    
    # outcome
    err1 <- rnorm(n, 0, 5)
    y1 <- commonMean + err1 + x7/0.5
    err2 <- rnorm(n, 0, 5)
    y2 <- commonMean + err2 + x8/0.5
    err3 <- rnorm(n, 0, 5)
    y3 <- commonMean + err3 + x9/0.5
    
    extraPsi1 <- if(excludeX==7){
        2/0.5
    }else{
        x7/0.5
    }
    psi1 <- commonPsi + extraPsi1
    
    extraPsi2 <- if(excludeX==8){
        2/0.5
    }else{
        x8/0.5
    }
    psi2 <- commonPsi + extraPsi2
    
    extraPsi3 <- if(excludeX==9){
        2/0.5
    }else{
        x9/0.5
    }
    psi3 <- commonPsi + extraPsi3
    
    y <- cbind(y1,y2,y3)
    psi <- cbind(psi1,psi2,psi3)
    omega0 <- r2weight:::alphaHat(Y = y, psiHat.Pnv0 = psi)
    
    y_omega0 <- y%*%omega0
    psi_omega0 <- psi%*%omega0
    
    mse_omega0 <- mean((y_omega0 - psi_omega0)^2)
    var_omega0 <- var(y_omega0)
    
    return(
        1 - mse_omega0/var_omega0
    )
}

