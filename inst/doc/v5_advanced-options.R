## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, out.width = "75%", fig.dim = c(6,5),
                      fig.align = "center")
library(distfreereg)

## ----upperlower, message = FALSE----------------------------------------------
set.seed(20240913)
n <- 1e2
func_upper <- function(X, theta) theta[1] + theta[2]*X[,1] + theta[3]*X[,2]^2
func_lower <- function(x, theta) theta[1] + theta[2]*x[1] +
  theta[3]*integrate(function(x) 2*x, lower = 0, upper = x[2])$value
theta <- c(2,5,1)
X <- matrix(rexp(2*n, rate = 1), ncol = 2)
Y <- distfreereg:::f2ftheta(f = func_upper, X)(theta) + rnorm(n)

set.seed(20240913)
dfr_upper <-  distfreereg(Y = Y, X = X, test_mean = func_upper,
                          covariance = list(Sigma = 1), theta_init = c(1,1,1))
set.seed(20240913)
dfr_lower <-  distfreereg(Y = Y, X = X, test_mean = func_lower,
                          covariance = list(Sigma = 1), theta_init = c(1,1,1))

all.equal(dfr_upper$observed_stats, dfr_lower$observed_stats)
all.equal(dfr_upper$p, dfr_lower$p)

## ----ordering, message = FALSE------------------------------------------------
set.seed(20241001)
n <- 1e2
func <- function(X, theta) theta[1] + theta[2]*X[,1]
theta <- c(2,5)
X <- matrix(rexp(n))

cdfr_simplex <- compare(true_mean = func,
                        true_X = X,
                        true_covariance = list(Sigma = 1),
                        theta = theta,
                        test_mean = func,
                        covariance = list(Sigma = 1),
                        X = X,
                        theta_init = c(1,1),
                        keep = 1)
cdfr_asis <- update(cdfr_simplex, ordering = "asis")

## ----ordering_plots-----------------------------------------------------------
plot(cdfr_simplex)
plot(cdfr_asis)

## ----change_mean, message = FALSE---------------------------------------------
alt_func <- function(X, theta) theta[1] + theta[2]*X[,1] + 0.5*X[,1]^2
cdfr_simplex_alt <- update(cdfr_simplex, true_mean = alt_func)
cdfr_asis_alt <- update(cdfr_asis, true_mean = alt_func)

## ----ordering_plots_alt-------------------------------------------------------
plot(cdfr_simplex_alt)
plot(cdfr_asis_alt)

## ----powers-------------------------------------------------------------------
rejection(cdfr_simplex_alt, stat = "KS")
rejection(cdfr_asis_alt, stat = "KS")

## ----epsp_null----------------------------------------------------------------
plot(cdfr_simplex$dfrs[[1]], which = "epsp")

## ----epsp_alt_asis------------------------------------------------------------
plot(cdfr_asis_alt$dfrs[[1]], which = "epsp")

## ----epsp_alt_simplex---------------------------------------------------------
plot(cdfr_simplex_alt$dfrs[[1]], which = "epsp")

## ----fitted_and_observed------------------------------------------------------
dfr_simplex <- cdfr_simplex_alt$dfrs[[1]]
X <- dfr_simplex$data$X
Y <- dfr_simplex$data$Y[order(X)]
Y_hat <- fitted(dfr_simplex)[order(X)]
X <- sort(X)
ml <- loess(Y ~ X)
plot(X, predict(ml), type = "l", col = "blue", ylab = "Y")
points(X, Y, col = rgb(0,0,1,0.4))
lines(X, Y_hat, col = "red", lty = "dashed")
legend(x = "bottomright", legend = c("smoothed", "fitted"), col = c("blue", "red"),
       lty = c("solid", "dashed"))

## ----optimal, message = FALSE-------------------------------------------------
cdfr_optimal_alt <- update(cdfr_simplex_alt, ordering = "optimal")
plot(cdfr_optimal_alt)
rejection(cdfr_optimal_alt, stat = "KS")
plot(cdfr_optimal_alt$dfrs[[1]], which = "epsp")

## ----natural, message = FALSE-------------------------------------------------
set.seed(20241003)
n <- 1e2
theta <- c(2,5,-1)
X <- cbind(sample(1:5, size = n, replace = TRUE),
           sample(1:10, size = n, replace = TRUE))
Y <- theta[1] + theta[2]*X[,1] + theta[3]*X[,2] + (2e-1)*X[,2]^2 + rnorm(nrow(X))

func <- function(X, theta) theta[1] + theta[2]*X[,1] + theta[3]*X[,2]

dfr <- distfreereg(Y = Y, X = X, test_mean = func, theta_init = c(1,1,1),
                   covariance = list(Sigma = 1), ordering = "natural")
dfr

## ----compare_with_simplex, message = FALSE------------------------------------
update(dfr, ordering = "simplex")

## ----group, message = FALSE---------------------------------------------------
dfr_grouped <- update(dfr, group = TRUE)

## ----length_of_epsp-----------------------------------------------------------
length(dfr_grouped$epsp)
length(which(table(X[,1], X[,2]) > 0))

## ----dfr_grouped_results------------------------------------------------------
dfr_grouped

## ----override, message = FALSE------------------------------------------------
set.seed(20241003)
n <- 2e2
func <- function(X, theta) theta[1] + theta[2]*X[,1]
theta <- c(2,5)
X <- matrix(rexp(n))

cdfr <- compare(true_mean = func, test_mean = func, true_X = X, X = X,
                true_covariance = list(Sigma = 1), covariance = list(Sigma = 1),
                theta = theta, theta_init = c(1,1))
cdfr_theta <- update(cdfr, global_override = list(theta_hat = theta))

## ----cdfr_plot----------------------------------------------------------------
plot(cdfr)

## ----cdfr_theta_plot----------------------------------------------------------
plot(cdfr_theta)

