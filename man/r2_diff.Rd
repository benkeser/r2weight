% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/r2Diff.R
\name{r2_diff}
\alias{r2_diff}
\title{r2_diff}
\usage{
r2_diff(object1, object2, comparison = c("diff", "ratio"), alpha = 0.05)
}
\arguments{
\item{object1}{An object of either class \code{optWeight} or of class 
\code{r2_optWeight}. The class type should match that of \code{object2}.}

\item{object2}{An object of either class \code{optWeight} or of class 
\code{r2_optWeight}. The class type should match that of \code{object1}.}

\item{comparison}{What type of comparison should be made. Possible choices include
\code{"diff"} and \code{"ratio"}.}

\item{alpha}{The function returns a \code{(1-alpha)*100} percent confidence interval. Default
is set to \code{0.05} (i.e., 95 percent confidence interval)}
}
\value{
Point estimate and confidence interval for the selected \code{comparison}.
}
\description{
Compare the R-squared values in two objects class \code{optWeight} or of 
class \code{r2_optWeight}. The former compares the R-squared values
for each outcome between the two \code{optWeight} objects, while the latter 
compares the R-squared values for the combined outcome of two \code{r2_optWeight}
objects.
}
\examples{
X <- data.frame(x1=runif(n=100,0,5), x2=runif(n=100,0,5))
Y1 <- rnorm(100, X$x1 + X$x2, 1)
Y2 <- rnorm(100, X$x1 + X$x2, 3)
Y <- data.frame(Y1 = Y1, Y2 = Y2)
#fit1 <- optWeight(Y = Y, X = X, SL.library = c("SL.glm","SL.mean"), 
#family = "gaussian", return.CV.SuperLearner = FALSE)
#perf.fit1 <- r2_optWeight(object = fit1, Y = Y, X = X, evalV = 5)
#fit2 <- optWeight(Y = Y, X = X[,1,drop=FALSE], SL.library = c("SL.glm","SL.mean"), 
#family = "gaussian",return.CV.SuperLearner = FALSE)
#perf.fit2 <- r2_optWeight(object = fit2, Y = Y, X = X[,1,drop=FALSE], evalV = 5)

# compare cross-validated r-squared for each outcome
#comp <- r2_diff(fit1, fit2)
# comp
# compare cross-validated r-squared for combined outcome
#perf.comp <- r2_diff(perf.fit1, perf.fit2)
# perf.comp
}
