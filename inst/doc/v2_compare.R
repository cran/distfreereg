## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, out.width = "75%", fig.dim = c(6,5),
                      fig.align = "center")
library(distfreereg)

## ----comp_1, message = FALSE--------------------------------------------------
set.seed(20240913)
n <- 10
func <- function(X, theta) theta[1] + theta[2]*X[,1] + theta[3]*X[,2]
theta <- c(2,5,-1)
X <- matrix(rexp(2*n, rate = 1), nrow = n)
comp_dfr <- compare(theta = theta, true_mean = func, test_mean = func,
                    true_X = X, true_covariance = list(Sigma = 3), X = X,
                    covariance = list(Sigma = 3),
                    theta_init = rep(1, length(theta)))

## ----cdf----------------------------------------------------------------------
plot(comp_dfr)

## ----qq-----------------------------------------------------------------------
plot(comp_dfr, which = "qq")

## ----qqp----------------------------------------------------------------------
plot(comp_dfr, which = "qqp")

## ----ks.test------------------------------------------------------------------
ks.test(comp_dfr)

## -----------------------------------------------------------------------------
names(comp_dfr$obs)
ks.test(comp_dfr, stat = "CvM")

## ----comp_larger_n, message = FALSE-------------------------------------------
n_2 <- 100
X_2 <- matrix(rexp(2*n_2, rate = 1), nrow = n_2)
comp_dfr_2 <- update(comp_dfr, true_X = X_2, X = X_2)

## ----qqplots_2----------------------------------------------------------------
plot(comp_dfr_2)
plot(comp_dfr_2, which = "qq")
plot(comp_dfr_2, which = "qqp")

## ----ks.test_2----------------------------------------------------------------
ks.test(comp_dfr_2)

## ----power, message = FALSE---------------------------------------------------
alt_func <- function(X, theta) theta[1] + theta[2]*X[,1] + theta[3]*X[,2] + 0.5*X[,2]^2
comp_dfr_3 <- update(comp_dfr_2, true_mean = alt_func, true_covariance = list(Sigma = 3))

## -----------------------------------------------------------------------------
plot(comp_dfr_3)

## -----------------------------------------------------------------------------
rejection(comp_dfr_3)

## -----------------------------------------------------------------------------
rejection(comp_dfr_3, alpha = 0.01)

