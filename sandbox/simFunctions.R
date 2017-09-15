#' makeData
#' 
#' Makes data for simulation. 
#' 
#' @param n Sample size
#' 
#' @export
#' @importFrom stats runif rbinom rnorm var 

makeData <- function(n){
    x1 <- stats::runif(n,0,4)
    x2 <- stats::runif(n,0,4)
    x3 <- stats::runif(n,0,4)
    x4 <- stats::rbinom(n,1,0.75)
    x5 <- stats::rbinom(n,1,0.25)
    x6 <- stats::rbinom(n,1,0.5)
    x7 <- stats::runif(n,0,4)
    x8 <- stats::runif(n,0,4)
    x9 <- stats::runif(n,0,4)
    
    commonMean <- x1 + x2/0.5 + x3/0.25 + x4 + x5/0.5 + x6/0.25
    
    # first outcome
    err1 <- stats::rnorm(n, 0, 5)
    y1 <- commonMean + err1 + x7/0.5
    
    # second outcome
    err2 <- stats::rnorm(n, 0, 5)
    y2 <- commonMean + err2 + x8/0.5
    
    # third outcome
    err3 <- stats::rnorm(n, 0, 5)
    y3 <- commonMean + err3 + x9/0.5
    
  return(list(Y=data.frame(y1=y1,y2=y2,y3=y3), 
              X=data.frame(x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,x6=x6,x7=x7,x8=x8,x9=x9)))
}

#' getTrueWeights
#' 
#' Compute the true value of the optimal weights for simulation 1.
#' 
#' @param n Sample size
#' @export

getTrueWeights <- function(n = 1e6){
    x1 <- stats::runif(n,0,4)
    x2 <- stats::runif(n,0,4)
    x3 <- stats::runif(n,0,4)
    x4 <- stats::rbinom(n,1,0.75)
    x5 <- stats::rbinom(n,1,0.25)
    x6 <- stats::rbinom(n,1,0.5)
    x7 <- stats::runif(n,0,4)
    x8 <- stats::runif(n,0,4)
    x9 <- stats::runif(n,0,4)
    
    commonMean <- x1 + x2/0.5 + x3/0.25 + x4 + x5/0.5 + x6/0.25
    
    # first outcome
    err1 <- stats::rnorm(n, 0, 5)
    y1 <- commonMean + err1 + x7/0.5
    
    # second outcome
    err2 <- stats::rnorm(n, 0, 5)
    y2 <- commonMean + err2 + x8/0.5
    
    # third outcome
    err3 <- stats::rnorm(n, 0, 5)
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
#' @param n Sample size
#' @export

getTrueOptR2 <- function(n=1e6){
    x1 <- stats::runif(n,0,4)
    x2 <- stats::runif(n,0,4)
    x3 <- stats::runif(n,0,4)
    x4 <- stats::rbinom(n,1,0.75)
    x5 <- stats::rbinom(n,1,0.25)
    x6 <- stats::rbinom(n,1,0.5)
    x7 <- stats::runif(n,0,4)
    x8 <- stats::runif(n,0,4)
    x9 <- stats::runif(n,0,4)
    
    commonMean <- x1 + x2/0.5 + x3/0.25 + x4 + x5/0.5 + x6/0.25
    
    # first outcome
    err1 <- stats::rnorm(n, 0, 5)
    y1 <- commonMean + err1 + x7/0.5
    
    # second outcome
    err2 <- stats::rnorm(n, 0, 5)
    y2 <- commonMean + err2 + x8/0.5
    
    # third outcome
    err3 <- stats::rnorm(n, 0, 5)
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
    var_omega0 <- stats::var(y_omega0)
    
    return(
        1 - mse_omega0/var_omega0
    )
}


#' getTrueUnivariateR2
#' 
#' Compute the true value of R^2_{0,j}. The code computes it only for 
#' y1 (from \code{makeData}), but it will be the same for all outcomes
#' @param n Sample size
#' @export

getTrueUnivariateR2 <- function(n=1e6){
    x1 <- stats::runif(n,0,4)
    x2 <- stats::runif(n,0,4)
    x3 <- stats::runif(n,0,4)
    x4 <- stats::rbinom(n,1,0.75)
    x5 <- stats::rbinom(n,1,0.25)
    x6 <- stats::rbinom(n,1,0.5)
    x7 <- stats::runif(n,0,4)

    commonMean <- x1 + x2/0.5 + x3/0.25 + x4 + x5/0.5 + x6/0.25
    
    # first outcome
    err1 <- stats::rnorm(n, 0, 5)
    y1 <- commonMean + err1 + x7/0.5
    
    psi <- commonMean + x7/0.5
    mse <- mean((y1-psi)^2)
    return(
        1 - mse/stats::var(y1)
    )
}



#' getTrueUnivariateR2Exclude
#' 
#' Compute the true value of R^2_{0,j} when a given variable of X is excluded.
#' 
#' @param n Sample size
#' @param excludeX What X column to exclude
#' @param whichY What Y column to study
#' @export
#' 

getTrueUnivariateR2Exclude <- function(n=1e6, excludeX, whichY){
    if(excludeX < 0 | excludeX > 9){
        stop("excludeX between 1-9")
    }
    if(whichY < 0 | whichY > 3){
        stop("whichY between 1-3")
    }
    x1 <- stats::runif(n,0,4)
    x2 <- stats::runif(n,0,4)
    x3 <- stats::runif(n,0,4)
    x4 <- stats::rbinom(n,1,0.75)
    x5 <- stats::rbinom(n,1,0.25)
    x6 <- stats::rbinom(n,1,0.5)
    x7 <- stats::runif(n,0,4)
    x8 <- stats::runif(n,0,4)
    x9 <- stats::runif(n,0,4)
    
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
    err <- stats::rnorm(n, 0, 5)
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
        1 - mse/stats::var(y)
    )
}



#' getTrueOptR2Exclude
#' 
#' Compute the true value of R^2_{0,omega_0} when a given variable of X is excluded. 
#' @param n Sample size
#' @param excludeX What X column to drop
#' 
#' @export
#' 

getTrueOptR2Exclude <- function(n=1e6, excludeX){
    if(excludeX < 0 | excludeX > 9){
        stop("excludeX between 1-9")
    }

    x1 <- stats::runif(n,0,4)
    x2 <- stats::runif(n,0,4)
    x3 <- stats::runif(n,0,4)
    x4 <- stats::rbinom(n,1,0.75)
    x5 <- stats::rbinom(n,1,0.25)
    x6 <- stats::rbinom(n,1,0.5)
    x7 <- stats::runif(n,0,4)
    x8 <- stats::runif(n,0,4)
    x9 <- stats::runif(n,0,4)
    
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
    err1 <- stats::rnorm(n, 0, 5)
    y1 <- commonMean + err1 + x7/0.5
    err2 <- stats::rnorm(n, 0, 5)
    y2 <- commonMean + err2 + x8/0.5
    err3 <- stats::rnorm(n, 0, 5)
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
    omega0 <- alphaHat(Y = y, psiHat.Pnv0 = psi)
    
    y_omega0 <- y%*%omega0
    psi_omega0 <- psi%*%omega0
    
    mse_omega0 <- mean((y_omega0 - psi_omega0)^2)
    var_omega0 <- stats::var(y_omega0)
    
    return(
        1 - mse_omega0/var_omega0
    )
}

#' plotSimResults
#' 
#' Function to plot bias and coverage from simulations
#'
#'@param out A \code{data.frame} of output that results from running \code{sce.sh} 
#'@param outExclude A \code{data.frame} of output that results from running \code{sce2.sh}
#'@param saveDir A \code{character} specifying the save directory
#'@param outFile A \code{character} specifying the .pdf file name for simulation 1 results
#'@param outExFile A \code{character} specifying the .pdf file name for simulation 2 results
#'@param ... Passed to \code{pdf}
#' 
#' @importFrom grDevices pdf dev.off
#' @importFrom graphics plot par segments abline axis layout

plotSimResults <- function(out, outExclude, ...,
                           saveDir = "~/Dropbox/Job Applications/Job Talk/",
                           outFile="sim1Rslt.pdf", outExFile="sim2Rslt.pdf"){
    #----------------
    # results for r2
    #----------------
    bias.r2 <- c(unlist(by(out, out$n, function(x){ mean(x$errOptR2,na.rm=TRUE)/x$trueOptR2[1]*100 })))
    cov.r2 <- c(unlist(by(out, out$n, function(x){ mean(x$covOptR2,na.rm=TRUE) })))
    se.cov.r2 <- sqrt(cov.r2*(1-cov.r2)/1000)
    #----------------------
    # results for delta-r2
    #----------------------
    bias.dr2.x2 <- c(unlist(by(outExclude[outExclude$excludeX==2,], outExclude$n[outExclude$excludeX==2], function(x){ mean(x$errOptR2.diff,na.rm=TRUE)/x$trueOptR2.diff[1]*100 })))
    cov.dr2.x2 <- c(unlist(by(outExclude[outExclude$excludeX==2,], outExclude$n[outExclude$excludeX==2], function(x){ mean(x$covOptR2.diff,na.rm=TRUE) })))
    se.cov.dr2.x2 <- sqrt(cov.r2*(1-cov.r2)/1000)
    
    # to plot excluding x7 as well uncomment this
    # bias.dr2.x7 <- c(unlist(by(outExclude[outExclude$excludeX==7,], outExclude$n[outExclude$excludeX==7], function(x){ mean(x$errOptR2.diff)/x$trueOptR2.diff[1]*100 })))
    # cov.dr2.x7 <- c(unlist(by(outExclude[outExclude$excludeX==7,], outExclude$n[outExclude$excludeX==7], function(x){ mean(x$covOptR2.diff) })))
    # se.cov.dr2.x7 <- sqrt(cov.r2*(1-cov.r2)/1000)
    #--------------------
    # sample sizes
    #--------------------
    n <- c(100,500,1000,5000)
    
    # r2 plots
    grDevices::pdf(paste0(saveDir,outFile),height = 2.6, width = 5.5)
    graphics::layout(matrix(1:2,nrow=1,byrow = TRUE))
    graphics::par(oma = c(0,0,0,0),
        mar = c(3.1, 3.1, 1, 1),
        mgp = c(1.5, 0.6, 0))
    graphics::plot(bias.r2 ~ I(1:4), xlab = "n", ylab="Bias (% of truth)", xaxt = "n", bty="n",
         ylim = c(-2, 1), type="b")
    graphics::axis(side = 1, at = 1:4, labels = prettyNum(n,big.mark = ","))
    graphics::abline(h=0, lty=3)
    
    graphics::plot(I(cov.r2*100) ~ I(1:4), xlab = "n", ylab="Coverage (MC CI)", 
         xaxt = "n", bty="n", type="b", ylim = c(87, 100))
    graphics::axis(side = 1, at = 1:4, labels = prettyNum(n,big.mark = ","))
    graphics::abline(h=95, lty=3)
    graphics::segments(x0=1:4, y0=100*(cov.r2-1.96*se.cov.r2), y1=100*(cov.r2+1.96*se.cov.r2))
    grDevices::dev.off()
    
    grDevices::pdf(paste0(saveDir,outExFile), height=2.6, width = 5.5)
    graphics::layout(matrix(1:2,nrow=1,byrow = TRUE))
    graphics::par(oma = c(0,0,0,0),
        mar = c(3.1, 3.1, 1, 1),
        mgp = c(1.5, 0.6, 0))
    graphics::plot(bias.dr2.x2 ~ I(1:4), xlab = "n", ylab="Bias (% of truth)", xaxt = "n", bty="n",
         ylim = c(-1,5), type="b", pch = 2)
    # points(bias.dr2.x7 ~I(1:4), type ="b", pch=5)
    graphics::axis(side = 1, at = 1:4, labels = prettyNum(n,big.mark = ","))
    graphics::abline(h=0, lty=3)
    # legend(x=2, y=0.02, bty="n", pch = c(2,5), 
    #        legend=c(expression("exclude "*X[2]), expression("exclude "*X[4])))
    
    graphics::plot(I(cov.dr2.x2*100) ~ I(1:4), xlab = "n", ylab="Coverage (MC CI)", 
         xaxt = "n", bty="n", type="b", ylim = c(80, 100), pch=2)
    # points(I(cov.dr2.x7*100) ~ I(1:4), type ="b", pch=5)
    graphics::axis(side = 1, at = 1:4, labels = prettyNum(n,big.mark = ","))
    graphics::abline(h=95, lty=3)
    graphics::segments(x0=1:4, y0=100*(cov.dr2.x2-1.96*se.cov.dr2.x2), y1=100*(cov.dr2.x2+1.96*se.cov.dr2.x2))
    # graphics::segments(x0=1:4, y0=100*(cov.dr2.x7-1.96*se.cov.dr2.x7), y1=100*(cov.dr2.x7+1.96*se.cov.dr2.x7))
    grDevices::dev.off()
    
    print("files saved")
}

