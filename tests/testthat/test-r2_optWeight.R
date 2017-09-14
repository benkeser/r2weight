library(r2weight)

context("Testing optWeight function")

test_that("optWeight works as expected.", {
	# sample size
	n <- 100

	# set the seed
	set.seed(12345)

	# simulate nine covariate predictors
	x1 <- runif(n,0,4)
	x2 <- runif(n,0,4)
	x3 <- runif(n,0,4)

	# put all predictors in single data.frame
	X <- data.frame(x1=x1, x2=x2, x3=x3)

	# simulate three outcomes
	y1 <- x1 + 2*x2 + 4*x3 + rnorm(n, 0, 5)
	y2 <- x1 + 2*x2 + 4*x3 + rnorm(n, 0, 5)
	y3 <- x1 + 2*x2 + 4*x3 + rnorm(n, 0, 5)
	# put all outcomes in single data.frame
	Y <- data.frame(y1 = y1, y2 = y2, y3 = y3)

	# call optWeight using simple Super Learner library
	out1 <- optWeight(Y = Y, X = X, SL.library = c("SL.mean","SL.glm"))

	# cross-validated R-squared
	# set verbose = TRUE to see a progress bar
	r2.out1 <- r2_optWeight(out1, Y = Y, X = X, evalV = 2, verbose = TRUE)

	expect_true(!is.na(r2.out1$r2))
	expect_true(all(!is.na(r2.out1$r2.ci)))
	expect_true(!is.na(r2.out1$r2.pval))
})