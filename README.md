Installation
------------

The package can be installed directly from GitHub as follows:

    # need the devtools package
    library("devtools")

    ## Warning: package 'devtools' was built under R version 3.2.5

    # install from GitHub
    install_github("benkeser/r2weight")

    # load package
    library("r2weight")

Use
---

Here we provide a few examples of simple calls to the `r2weight`
function. The function is designed to learn the optimal weighted
combination of outcomes that can be predicted using a given set of
variables. The `r2weight` function relies on initial fits from the
`CV.SuperLearner` function from the `SuperLearner` R package available
on CRAN. This demonstration will assume that the user is familiar with
executing calls to `CV.SuperLearner`. We refer readers to the
`SuperLearner` package documentation for additional information.

Suppose we have `X`, a `data.frame` of three variables that we would
like to use to predict the value of two related outcomes `Y1` and `Y2`:

    set.seed(1234)
    # simulate a data.frame
    X <- data.frame(x1 = runif(100, 0, 1),
                    x2 = rnorm(100, 0, 1), 
                    x3 = rbinom(100, 1, 0.5))

    Y1 <- rnorm(100,mean=X$x1 + X$x2 + X$x3,sd=1)
    Y2 <- rnorm(100,mean=X$x1 + X$x2 + X$x3,sd=2)

The two outcomes are related to `X` in the same way; however, `Y2` is
more variable. We could use the function `SuperLearner` to perform
cross-validaiton-based ensemble machine learning of the optimal
prediction function for predicting `Y1` from `X` and for predicting `Y2`
from `X`. The function `CV.SuperLearner` provides a means of assessing
the performance of these predictions. In particular, `CV.SuperLearner`
uses V-fold cross-validation to evaluate the performance of
`SuperLearner` for predicting the outcomes separately. The function
`r2weight` uses the fits from `CV.SuperLearner` to determine the optimal
way of weighting the responses `Y1` and `Y2` such that the predictive
power (as measured by cross-validated *R*<sup>2</sup>) is maximized.

To apply the function `r2weight` requires several specific steps when
executing the call to `CV.SuperLearner`. We must set the options
`returnAll = TRUE` (default) and `control$saveFitLibrary = TRUE` (not
default). Furthermore, we must ensure that the splits used in the outer
V-fold cross-validation step are the same. This can be achieved by
setting the same seed immediately before each call to `CV.SuperLearner`.
We illustrate below using a simple example:

    library(SuperLearner)

    ## Loading required package: nnls

    ## Super Learner

    ## Version: 2.0-19

    ## Package created on 2016-02-02

    # set a seed
    set.seed(1234)
    # execute call to CV.SuperLearner for predicting Y1
    cvsl1 <- CV.SuperLearner(
        Y=Y1, # outcome = Y1
        X=X, # prediction frame
        SL.library = c("SL.glm","SL.step","SL.mean"), # simple library
        control=list(saveFitLibrary = TRUE), # needed for r2weight
        saveAll = TRUE # not needed because it's default option, 
                       # but include for clarity
    )

    # set the same seed to ensure same sample split
    set.seed(1234)
    # execute call to CV.SuperLearner for predicting Y2
    cvsl2 <- CV.SuperLearner(
        Y=Y2, # outcome = Y2
        X=X, # prediction frame
        SL.library = c("SL.glm","SL.step","SL.mean"), # simple library
        control=list(saveFitLibrary = TRUE), # needed for r2weight
        saveAll = TRUE # not needed because it's default option, 
                       # but include for clarity
    )

We can now execute a call to `r2weight` using these objects to obtain
the estimated cross-validated *R*<sup>2</sup> for the optimally combined
outcome:

    # execute the function
    r2Fit <- r2weight(cvslList = list(cvsl1, cvsl2), X=list(X))
    # print the output
    r2Fit

    ## CV-R2 (95% CI) = 0.531 (0.257 , 0.704)
    ## 
    ## Summary across folds: 
    ##       w.R2    w1    w2
    ## 1   0.933 0.828 0.172
    ## 2  -0.093 0.816 0.184
    ## 3   0.432 0.838 0.162
    ## 4  -0.732 0.810 0.190
    ## 5   0.794 0.818 0.182
    ## 6   0.252 0.836 0.164
    ## 7   0.526 0.854 0.146
    ## 8   0.770 0.823 0.177
    ## 9   0.235 0.835 0.165
    ## 10  0.744 0.859 0.141
    ## 11  0.732 0.836 0.164
    ## 12  0.702 0.818 0.182
    ## 13  0.887 0.818 0.182
    ## 14  0.526 0.806 0.194
    ## 15  0.112 0.824 0.176
    ## 16  0.650 0.828 0.172
    ## 17  0.379 0.834 0.166
    ## 18  0.356 0.839 0.161
    ## 19  0.539 0.769 0.231
    ## 20 -0.539 0.799 0.201

    # cross-validated R-squared for predicting Y1 and confidence interval
    r2Fit$r2[[1]]$cv.wR2

    ## [1] 0.5047084

    r2Fit$r2[[1]]$cv.wR2.ci

    ## [1] 0.2698801 0.6640090

    # cross-validated R-squared for predicting Y2 and confidence interval
    r2Fit$r2[[2]]$cv.wR2

    ## [1] 0.1942642

    r2Fit$r2[[2]]$cv.wR2.ci

    ## [1] -0.05146179  0.38256410

Here's an example where we use different predictors to predict `Y1` and
`Y2`:

    set.seed(1234)
    # simulate a data.frame
    X <- data.frame(x1 = runif(100, 0, 1),
                    x2 = rnorm(100, 0, 1), 
                    x3 = rbinom(100, 1, 0.5))

    Y1 <- rnorm(100,mean=X$x1 + X$x2,sd=1)
    Y2 <- rnorm(100,mean=X$x2 + X$x3,sd=1)

    library(SuperLearner)
    # set a seed
    set.seed(1234)
    # execute call to CV.SuperLearner for predicting Y1
    cvsl3 <- CV.SuperLearner(
        Y=Y1, # outcome = Y1
        X=X[,c(1,2)], # only use x1 and x2 to predict
        SL.library = c("SL.glm","SL.step","SL.mean"), # simple library
        control=list(saveFitLibrary = TRUE), # needed for r2weight
        saveAll = TRUE # not needed because it's default option, 
                       # but include for clarity
    )

    # set the same seed to ensure same sample split
    set.seed(1234)
    # execute call to CV.SuperLearner for predicting Y2
    cvsl4 <- CV.SuperLearner(
        Y=Y2, # outcome = Y2
        X=X[,c(2,3)], # only use x2 and x3
        SL.library = c("SL.glm","SL.step","SL.mean"), # simple library
        control=list(saveFitLibrary = TRUE), # needed for r2weight
        saveAll = TRUE # not needed because it's default option, 
                       # but include for clarity
    )

    # execute the function
    r2Fit2 <- r2weight(cvslList = list(cvsl3, cvsl4), 
                      X=list(X[,c(1,2)], X[,c(2,3)]) # list of each frame used
                      )
    # print the output
    r2Fit2

    ## CV-R2 (95% CI) = 0.57 (0.217 , 0.764)
    ## 
    ## Summary across folds: 
    ##       w.R2    w1    w2
    ## 1   0.938 0.478 0.522
    ## 2  -0.467 0.448 0.552
    ## 3   0.520 0.486 0.514
    ## 4  -1.064 0.471 0.529
    ## 5   0.797 0.462 0.538
    ## 6   0.545 0.508 0.492
    ## 7   0.671 0.521 0.479
    ## 8   0.667 0.453 0.547
    ## 9  -0.029 0.491 0.509
    ## 10  0.945 0.514 0.486
    ## 11  0.866 0.469 0.531
    ## 12  0.436 0.449 0.551
    ## 13  0.935 0.444 0.556
    ## 14  0.490 0.455 0.545
    ## 15  0.448 0.477 0.523
    ## 16  0.738 0.463 0.537
    ## 17  0.506 0.521 0.479
    ## 18  0.514 0.489 0.511
    ## 19  0.012 0.355 0.645
    ## 20 -2.635 0.438 0.562

Here's an example showing that we can use `r2weight` even when there is
just a single outcome to predict. Thus, the function can be used as
obtaining inference on cross-validated *R*<sup>2</sup> after fitting
`CV.SuperLearner`:

    r2FitSingle <- r2weight(cvslList = list(cvsl1), X=list(X))
    r2FitSingle

    ## CV-R2 (95% CI) = 0.505 (0.27 , 0.664)
    ## 
    ## Summary across folds: 
    ##       w.R2 w1
    ## 1   0.921  1
    ## 2   0.155  1
    ## 3  -0.066  1
    ## 4  -0.400  1
    ## 5   0.816  1
    ## 6   0.101  1
    ## 7   0.418  1
    ## 8   0.796  1
    ## 9   0.088  1
    ## 10 -0.926  1
    ## 11  0.580  1
    ## 12  0.608  1
    ## 13  0.822  1
    ## 14  0.655  1
    ## 15  0.220  1
    ## 16  0.548  1
    ## 17  0.346  1
    ## 18  0.222  1
    ## 19  0.973  1
    ## 20  0.301  1

We can also get the performance of a single algorithm used by the Super
Learner:

    # 
    r2FitGLM <- r2weight(
        cvslList = list(cvsl1), X=list(X),
        whichAlgorithm = "SL.glm_All" # recall SuperLearner adds _All to each function
                                      # if no screening wrapper is specified
        )
    r2FitGLM

    ## CV-R2 (95% CI) = 0.504 (0.265 , 0.665)
    ## 
    ## Summary across folds: 
    ##       w.R2 w1
    ## 1   0.927  1
    ## 2   0.152  1
    ## 3  -0.094  1
    ## 4  -0.430  1
    ## 5   0.819  1
    ## 6   0.096  1
    ## 7   0.420  1
    ## 8   0.817  1
    ## 9   0.088  1
    ## 10 -1.094  1
    ## 11  0.586  1
    ## 12  0.625  1
    ## 13  0.828  1
    ## 14  0.655  1
    ## 15  0.221  1
    ## 16  0.555  1
    ## 17  0.334  1
    ## 18  0.221  1
    ## 19  0.977  1
    ## 20  0.293  1
