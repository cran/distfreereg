## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, out.width = "75%",
                      fig.dim = c(6,5), fig.align = "center")
library(distfreereg)

## ----lm_testing_with_plots----------------------------------------------------
set.seed(20240304)
n <- 3e2
form_lm <- y ~ x
data_lm <- data.frame(x = runif(n, min = 0, max = 3))
data_lm$y <- data_lm$x^2.1 + rnorm(n, sd = 0.3)
m_1 <- lm(form_lm, data = data_lm)
plot(m_1, which = 1)

## ----create_lm----------------------------------------------------------------
form_lm_2 <- y ~ I(x^2)
m_2 <- lm(form_lm_2, data = data_lm)
plot(m_2, which = 1)

## ----lm_method----------------------------------------------------------------
set.seed(20240304)
(dfr_lm_2 <- distfreereg(test_mean = m_2))

## ----create_nls---------------------------------------------------------------
set.seed(20240304)
n <- 3e2
sds <- runif(n, min = 0.5, max = 5)
data_nls <- data.frame(x = rnorm(n), y = rnorm(n))
data_nls$z <- exp(3*data_nls$x) - 2*data_nls$y^2 + rnorm(n, sd = sds)
form_nls <- z ~ exp(a*x) - b*y^2
m_2 <- nls(form_nls, data = data_nls, weights = 1/sds^2, start = c(a = 1, b = 1))

## ----nls_method---------------------------------------------------------------
set.seed(20240304)
(dfr_nls <- distfreereg(test_mean = m_2))

## ----formula_method_lm--------------------------------------------------------
set.seed(20240304)
(dfr_form_lm_2 <- distfreereg(test_mean = form_lm_2, data = data_lm))

## ----comparison_lm------------------------------------------------------------
identical(dfr_lm_2$observed_stats, dfr_form_lm_2$observed_stats)
identical(dfr_lm_2$p, dfr_form_lm_2$p)

## ----formula_method_nls-------------------------------------------------------
set.seed(20240304)
(dfr_form_nls <- distfreereg(test_mean = form_nls, data = data_nls, method = "nls",
                             covariance = list(P = diag(1/sds^2)),
                             theta_init = c(a = 1, b = 1)))

## ----comparison_nls-----------------------------------------------------------
identical(dfr_nls$observed_stats, dfr_form_nls$observed_stats)
identical(dfr_nls$p, dfr_form_nls$p)

## ----dfr_1--------------------------------------------------------------------
set.seed(20240304)
n <- 3e2
true_mean <- function(X, theta) exp(theta[1]*X[,1]) - theta[2]*X[,2]^2
test_mean <- true_mean
theta <- c(3,-2)
Sigma <- rWishart(1, df = n, Sigma = diag(n))[,,1]
X <- matrix(rnorm(2*n), nrow = n)
Y <- distfreereg:::f2ftheta(true_mean, X)(theta) +
  distfreereg:::rmvnorm(n = n, reps = 1, SqrtSigma = distfreereg:::matsqrt(Sigma))

(dfr_1 <- distfreereg(test_mean = test_mean, Y = Y, X = X,
                      covariance = list(Sigma = Sigma),
                      theta_init = rep(1, length(theta))))

## ----default_inputs-----------------------------------------------------------
J <- dfr_1$J
fitted_values <- fitted(dfr_1)

## ----default------------------------------------------------------------------
distfreereg(test_mean = NULL, Y = Y, X = X, fitted_values = fitted_values,
            J = J, covariance = list(Sigma = Sigma))

## -----------------------------------------------------------------------------
plot(dfr_1)

## ----residuals_plot-----------------------------------------------------------
plot(dfr_1, which = "residuals")

## ----epsp_plot----------------------------------------------------------------
plot(dfr_1, which = "epsp")

## ----dfr_2--------------------------------------------------------------------
Q <- distfreereg:::matsqrt(distfreereg:::matinv(Sigma, tol = .Machine$double.eps))
dfr_2 <- distfreereg(Y = Y, X = X, test_mean = test_mean,
                     covariance = list(Q = Q),
                     theta_init = rep(1, length(theta)))
identical(dfr_1$observed_stats, dfr_2$observed_stats)

## ----new_stat-----------------------------------------------------------------
new_stat <- function(x) sum(abs(x))
update(dfr_1, stat = "new_stat")

## ----three_stats--------------------------------------------------------------
update(dfr_1, stat = c("new_stat", "KS", "CvM"))

## ----stats_checks-------------------------------------------------------------
bad_stat <- function(x) x[1,2]
try(update(dfr_1, stat = c("KS", "CvM", "bad_stat")))
try(update(dfr_1, stat = c("KS", "CvM", "undefined_stat")))

## ----t_dist-------------------------------------------------------------------
set.seed(20241003)
n <- 1e2
func <- function(X, theta) theta[1] + theta[2]*X[,1] + 0.5*X[,1]^2
theta <- c(2,5)
X <- matrix(rexp(n))
Y <- theta[1] + theta[2]*X[,1] + 0.5*X[,1]^2 + rt(n, df = 3)

dfr <- distfreereg(Y = Y, X = X, test_mean = func, theta_init = c(1,1),
                   covariance = list(Sigma = 3))
dfr

alt_func <- function(X, theta) theta[1] + theta[2]*X[,1]
update(dfr, test_mean = alt_func)

