Introduction
------------

This package can be used to learn the convex combination of a
multivariate outcome that maximizes cross-validated R-squared of a Super
Learner-based prediction. Super Learner is an ensemble machine learning
method with an associated [R
package](https://github.com/ecpolley/SuperLearner) written by Eric
Polley. The `SuperLearner` package provides a flexible interface for
utilizing user-written wrappers to perform ensemble learning. A short
tutorial on use of the Super Learner package can be found
[here](http://benkeser.github.io/sllecture/).

In addition to estimating the best convex combination of outcomes for
the sake of prediction, the `r2weight` package simultaneously estimates
a function for predicting this learned convex combination and can
compute a cross-validated estimate of the R-squared of this prediction
algorithm for predicting the convex combination of outcomes. The package
also computes influence function-based estimates of the standard error
of the estimated performance metric that are used to construct
confidence intervals and hypotheses tests. Finally, the package includes
functions for computing variable importance measures for each variable.

Installation
------------

The package can be installed directly from GitHub as follows:

    # install/load devtools package
    if(!("devtools" %in% row.names(installed.packages()))){
        install.packages("devtools")
    }
    library("devtools")

    # install/load r2weight from GitHub
    if(!("r2weight" %in% row.names(installed.packages()))){
        install_github("benkeser/r2weight")
    }
    library("r2weight")

Use
---

The basic workflow of the package is to call the function `optWeight`,
which estimates the optimal weights for the combined outcome. The
function `r2.optWeight` can then be called using the `optWeight` object
to obtain a cross-validated estimate of the R-squared for predicting the
combined outcome.

Here we illustrate the method using simulated data.

    # sample size
    n <- 500

    # set the seed
    set.seed(12345)

    # simulate nine covariate predictors
    x1 <- runif(n,0,4)
    x2 <- runif(n,0,4)
    x3 <- runif(n,0,4)
    x4 <- rbinom(n,1,0.75)
    x5 <- rbinom(n,1,0.25)
    x6 <- rbinom(n,1,0.5)
    x7 <- runif(n,0,4)
    x8 <- runif(n,0,4)
    x9 <- runif(n,0,4)
    # put all predictors in single data.frame
    X <- data.frame(x1=x1, x2=x2, x3=x3, x4=x4, x5=x5, 
                    x6=x6, x7=x7, x8=x8, x9=x9)

    # simulate three outcomes
    y1 <- x1 + 2*x2 + 4*x3 + x4 + 2*x5 + 4*x6 + 2*x7 + rnorm(n, 0, 5)
    y2 <- x1 + 2*x2 + 4*x3 + x4 + 2*x5 + 4*x6 + 2*x8 + rnorm(n, 0, 5)
    y3 <- x1 + 2*x2 + 4*x3 + x4 + 2*x5 + 4*x6 + 2*x9 + rnorm(n, 0, 5)
    # put all outcomes in single data.frame
    Y <- data.frame(y1 = y1, y2 = y2, y3 = y3)

    # call optWeight using simple Super Learner library
    out1 <- optWeight(Y = Y, X = X, SL.library = c("SL.mean","SL.glm","SL.step"))

    # print the object
    out1

    ## 
    ## Optimal weights for prediction with SuperLearner  : 
    ## y1  :  0.371 
    ## y2  :  0.314 
    ## y3  :  0.315 
    ## 
    ##  
    ## R-squared for each outcome with SuperLearner  : 
    ##       R2  CI.l  CI.h
    ## y1 0.643 0.589 0.689
    ## y2 0.613 0.559 0.660
    ## y3 0.625 0.573 0.671

The estimated optimal weights are shown (note that the true weights in
this example are 1/3, 1/3, 1/3) along with the estimated cross validated
R-squared for predicting each outcome using the Super Learner. There is
an S3-method for predicting the combined outcome on new data.

    # generate new data
    newX <- data.frame(x1=1, x2=1, x3=1, x4=1, x5=1, 
                       x6=1, x7=1, x8=1, x9=1)
    # get prediction of combined outcome
    predict(out1, newdata = newX)

    ##          [,1]
    ## [1,] 15.80969

We can call `r2.optWeight` to get an estimate of the cross-validated
R-squared for predicting the combined outcome with Super Learner.

    # cross-validated R-squared
    # set verbose = TRUE to see a progress bar
    r2.out1 <- r2.optWeight(out1, Y = Y, X = X)

    # print the output
    r2.out1

    ## 
    ##  
    ## R-squared for each outcome with SuperLearner  : 
    ##       R2  CI.l  CI.h
    ## y1 0.643 0.589 0.689
    ## y2 0.613 0.559 0.660
    ## y3 0.625 0.573 0.671
    ## 
    ## 
    ##  
    ## R-squared for combined outcome with SuperLearner  : 
    ##                         R2  CI.l  CI.h
    ## weighted combination 0.815 0.784 0.841

The R-squared for each individual outcome is shown, as well as for the
combined outcome. In this example, the true R-squared for individual
outcomes is about 0.6 and for the combined outcome about 0.8.

Variable importance
-------------------

A measure of variable importance for a particular variable can be
defined as the difference in R-squared for the combined outcome when
including and excluding that variable. These measures can be estimated
using the `r2Diff` function.

    # measure importance of x9
    out2 <- optWeight(Y = Y, X = X[,1:8], SL.library = c("SL.glm","SL.mean","SL.step"))

    # print output
    out2 

    ## 
    ## Optimal weights for prediction with SuperLearner  : 
    ## y1  :  0.404 
    ## y2  :  0.349 
    ## y3  :  0.248 
    ## 
    ##  
    ## R-squared for each outcome with SuperLearner  : 
    ##       R2  CI.l  CI.h
    ## y1 0.643 0.590 0.690
    ## y2 0.614 0.561 0.661
    ## y3 0.525 0.463 0.581

    # compare to full fit to get importance for each individual outcome
    outDiff <- r2Diff(out1, out2)
    # difference in R-squared for first outcome
    # with confidence interval and p-value for two-sided test that 
    # the difference equals 0
    outDiff$y1$diff

    ##             est         CI.l         CI.h         p
    ## 1 -0.0006708281 -0.002014265 0.0006726093 0.3277368

    # for the third outcome
    outDiff$y3$diff

    ##          est       CI.l     CI.h            p
    ## 1 0.09986118 0.06798041 0.131742 8.290725e-10

    # get R-squared for combined outcome excluding x9
    r2.out2 <- r2.optWeight(out2, Y = Y, X = X[,1:8])

    # print output, notice change in weights
    r2.out2

    ## 
    ##  
    ## R-squared for each outcome with SuperLearner  : 
    ##       R2  CI.l  CI.h
    ## y1 0.643 0.590 0.690
    ## y2 0.614 0.561 0.661
    ## y3 0.525 0.463 0.581
    ## 
    ## 
    ##  
    ## R-squared for combined outcome with SuperLearner  : 
    ##                         R2  CI.l  CI.h
    ## weighted combination 0.804 0.772 0.832

    # compare to full fit to get importance for combined outcome
    outDiff.r2 <- r2Diff(r2.out1, r2.out2)

    # print output
    outDiff.r2

    ## $diff
    ##          est        CI.l       CI.h          p
    ## 1 0.01067995 0.002252427 0.01910748 0.01299872
    ## 
    ## $ratio
    ##         est      CI.l      CI.h          p
    ## 1 0.9454212 0.9049312 0.9877229 0.01196758
    ## 
    ## $type
    ## [1] "r2.optWeight"
    ## 
    ## $Ynames
    ## [1] "y1" "y2" "y3"
    ## 
    ## attr(,"class")
    ## [1] "r2.compare"
