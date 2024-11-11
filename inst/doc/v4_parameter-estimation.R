## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, out.width = "75%",
                      fig.dim = c(6,5), fig.align = "center")
library(distfreereg)

## ----dfr_1--------------------------------------------------------------------
set.seed(20240926)
n <- 3e2
true_mean <- function(X, theta) exp(theta[1]*X[,1]) - theta[2]*X[,2]^2
test_mean <- true_mean
theta <- c(3,-2)
Sigma <- rWishart(1, df = n, Sigma = diag(n))[,,1]
X <- matrix(rnorm(2*n), nrow = n)
Y <- distfreereg:::f2ftheta(true_mean, X)(theta) +
  distfreereg:::rmvnorm(n = n, reps = 1, SqrtSigma = distfreereg:::matsqrt(Sigma))

dfr_1 <- distfreereg(test_mean = test_mean, Y = Y, X = X,
                     covariance = list(Sigma = Sigma),
                     theta_init = rep(1, length(theta)))

## ----dfr_2--------------------------------------------------------------------
set.seed(20240926)
dfr_2 <- update(dfr_1, control = list(
  optimization_args = list(method = "L-BFGS-B", lower = c(2,-3), upper = c(4,-1))))

identical(dfr_1$theta_hat, dfr_2$theta_hat)
all.equal(dfr_1$theta_hat, dfr_2$theta_hat)

## ----vcov---------------------------------------------------------------------
vcov(dfr_1)

## ----confint------------------------------------------------------------------
confint(dfr_1, level = 0.9)

## ----nlm, message = FALSE-----------------------------------------------------
set.seed(20240926)
dfr_3 <- update(dfr_1, control = list(optimization_fun = nlminb,
                                      fun_to_optimize_arg = "objective",
                                      theta_init_arg = "start",
                                      theta_hat_name = "par"))

## -----------------------------------------------------------------------------
identical(dfr_1$theta_hat, dfr_3$theta_hat)
all.equal(dfr_1$theta_hat, dfr_3$theta_hat)

## -----------------------------------------------------------------------------
dfr_3$optimization_output

## ----optim_bad_theta_init-----------------------------------------------------
try(update(dfr_1, theta_init = 1))# starting parameter vector too short!
try(update(dfr_1, theta_init = rep(1, 3)))# starting parameter vector too long!

## ----block_diagonal_setup_function_parameters---------------------------------
true_func <- function(X, theta) theta[1] + theta[2]*X[,1] + X[,1]^2
theta <- c(2,5)

## ----block_diagonal_setup_err_dist--------------------------------------------
matdim <- 10
block_t <- function(n, reps, blocks, df){
  output <- matrix(NA, nrow = n, ncol = reps)
  for(i in seq_len(reps))
    output[,i] <- as.vector(sapply(blocks, function(x) mvtnorm::rmvt(1, df = df, sigma = x*(df-2)/df)))
  return(output)
}

## ----block_diagonal_define_compare--------------------------------------------
get_theta_hat <- function(dfr) dfr$theta_hat

create_cdfr <- function(n){
  X <- as.matrix(rexp(n, rate = 1))
  Sig_list <- lapply(1:(n/matdim), function(x) rWishart(1, df = matdim, diag(matdim))[,,1])
  Sig <- as.matrix(Matrix::bdiag(Sig_list))
  
  return(compare(theta = theta,
                 true_mean = true_func, test_mean = true_func,
                 true_X = X, X = X,
                 true_covariance = list(Sigma = Sig), covariance = list(Sigma = Sig),
                 theta_init = c(1,1),
                 err_dist_fun = block_t,
                 err_dist_args = list(blocks = Sig_list, df = 4),
                 reps = 1e3,
                 prog = Inf,
                 manual = get_theta_hat,
                 control = list(optimization_args = list(method = "Nelder-Mead")))
         )
}

## ----run_with_14--------------------------------------------------------------
set.seed(14)
cdfr_seed14 <- create_cdfr(400)
theta_seed14_1 <- sapply(cdfr_seed14$manual, function(x) x[[1]])
theta_seed14_2 <- sapply(cdfr_seed14$manual, function(x) x[[2]])

## ----cdf_plot-----------------------------------------------------------------
plot(cdfr_seed14)

## ----density_plots------------------------------------------------------------
plot(density(theta_seed14_1))
plot(density(theta_seed14_2))

## ----sample_size_not_the_problem----------------------------------------------
set.seed(1)
cdfr_seed1 <- create_cdfr(400)
theta_seed1_1 <- sapply(cdfr_seed1$manual, function(x) x[[1]])
theta_seed1_2 <- sapply(cdfr_seed1$manual, function(x) x[[2]])
plot(cdfr_seed1)
plot(density(theta_seed1_1))
plot(density(theta_seed1_2))

## ----kstest-------------------------------------------------------------------
ks.test(cdfr_seed1)

