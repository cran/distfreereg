## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, out.width = "75%", fig.dim = c(6,5),
                      fig.align = "center")
library(distfreereg)

## ----dfr, message = FALSE-----------------------------------------------------
set.seed(20240214)
n <- 1e2
true_mean <- function(X, theta) theta[1] + theta[2]*X[,1]
theta <- c(2,5)

X <- matrix(runif(n, min = 1, max = 100))
Y <- true_mean(X, theta) + rnorm(n)

dfr <- distfreereg(Y = Y, X = X, test_mean = true_mean,
                   covariance = list(Sigma = 1),
                   theta_init = rep(1, length(theta)))

## -----------------------------------------------------------------------------
plot(dfr)

## -----------------------------------------------------------------------------
plot(dfr, text_args = list(adj = c(0, 0.5), y = 0.5))

## -----------------------------------------------------------------------------
plot(dfr, text_args = FALSE)

## -----------------------------------------------------------------------------
plot(dfr, stat = "CvM", text_args = FALSE)

## -----------------------------------------------------------------------------
plot(dfr, text_args = FALSE, lty = 2)

## -----------------------------------------------------------------------------
plot(dfr, text_args = FALSE, abline_args = FALSE)

## -----------------------------------------------------------------------------
plot(dfr, text_args = FALSE, polygon_args = list(density = 10))

## -----------------------------------------------------------------------------
plot(dfr, text_args = FALSE, shade_col = rgb(0.5, 0.5, 0.8, 0.5))

## -----------------------------------------------------------------------------
plot(dfr, text_args = FALSE, abline_args = FALSE, polygon_args = list(border = NULL))

## ----distfreereg_returned_values----------------------------------------------
output <- plot(dfr, confband_args = NULL, text_args = FALSE)
names(output)

## -----------------------------------------------------------------------------
names(output$confband)

## ----residual_plot------------------------------------------------------------
plot(dfr, which = "residuals")

## ----modified_residual_plot---------------------------------------------------
plot(dfr, which = "residuals", main = "New Title", lty = "dashed")

## ----epsp_plot----------------------------------------------------------------
plot(dfr, which = "epsp")
plot(dfr, which = "epsp", xlab = "i", col = "red")

## ----reference_object, message = FALSE----------------------------------------
set.seed(20240920)
n <- 100
func <- function(X, theta) theta[1] + theta[2]*X[,1] + theta[3]*X[,2]
theta <- c(2,5,-1)
X <- matrix(rexp(2*n), nrow = n)
cdfr <- compare(theta = theta, true_mean = func, test_mean = func,
                true_X = X, true_covariance = list(Sigma = 3), X = X,
                covariance = list(Sigma = 3), prog = Inf,
                theta_init = rep(1, length(theta)))

## ----cdf----------------------------------------------------------------------
plot(cdfr)

## ----modify_curves------------------------------------------------------------
plot(cdfr, curve_args = list(lwd = 3))

## ----modify_curves_differently------------------------------------------------
plot(cdfr, curve_args = list(lwd = 3, obs = list(lty = 4)))

## ----modify_legend------------------------------------------------------------
plot(cdfr, legend = list(title = "A Title", bg = "grey"))

## ----omit_legend--------------------------------------------------------------
plot(cdfr, legend = FALSE)

## ----modify_horizontal_lines--------------------------------------------------
plot(cdfr, hlines = list(lty = 1, lwd = 3))

## ----confidence_bands---------------------------------------------------------
plot(cdfr, confband_args = NULL)

## ----density------------------------------------------------------------------
plot(cdfr, which = "dens")

## ----modify_shading-----------------------------------------------------------
plot(cdfr, which = "dens",
     poly = list(density = 20, obs = list(col = rgb(0.5,0.2,0.2,0.2), angle = -45)))

## ----qqplots------------------------------------------------------------------
plot(cdfr, which = "qq")
plot(cdfr, which = "qqp")

## ----modify_qqplots-----------------------------------------------------------
plot(cdfr, which = "qq", conf.level = 0.95)

## ----modify_diagonal_line-----------------------------------------------------
plot(cdfr, which = "qqp", qqline = list(lwd = 3))

## ----second_object, message = FALSE-------------------------------------------
set.seed(20240920)
n <- 100
func <- function(X, theta) theta[1] + theta[2]*X[,1]
theta <- c(7,3)
X <- matrix(rexp(n), nrow = n)
cdfr2 <- compare(theta = theta, true_mean = func, test_mean = func,
                 true_X = X, true_covariance = list(Sigma = 3), X = X,
                 covariance = list(Sigma = 3), prog = Inf,
                 theta_init = rep(1, length(theta)))

## ----compare_two_objects------------------------------------------------------
plot(cdfr, cdfr2)

## ----compare_returned_values--------------------------------------------------
output <- plot(cdfr, confband_args = NULL)
names(output)

## -----------------------------------------------------------------------------
names(output$confband_observed)

