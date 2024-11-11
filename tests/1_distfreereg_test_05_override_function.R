library(distfreereg)
set.seed(20240319)
n <- 1e2
func <- function(x, theta) theta[1] + theta[2]*x
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
theta <- c(2,5)
X <- matrix(rexp(n, rate = 1))
Y <- distfreereg:::f2ftheta(f = func, X)(theta) +
  as.vector(distfreereg:::rmvnorm(n = n, reps = 1, mean = rep(0,n), SqrtSigma = distfreereg:::matsqrt(Sig)))
set.seed(20240319)
dfrfunc_1 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig), theta_init = c(1,1), verbose = FALSE)
set.seed(20240319)
dfrfunc_2 <- distfreereg(Y = Y, X = X, test_mean = func, theta_init = c(1,1), verbose = FALSE,
                         covariance = list(Sigma = Sig), override = list(res_order = dfrfunc_1[["res_order"]]))
set.seed(20240319)
dfrfunc_3 <- distfreereg(Y = Y, X = X, test_mean = func, verbose = FALSE,
                         covariance = list(Sigma = Sig), override = list(theta_hat = dfrfunc_1[["theta_hat"]]))
set.seed(20240319)
dfrfunc_4 <- distfreereg(Y = Y, X = X, test_mean = func, theta_init = c(1,1), verbose = FALSE,
                         covariance = list(Sigma = Sig), override = list(r = dfrfunc_1[["r"]]))
set.seed(20240319)
dfrfunc_5 <- distfreereg(Y = Y, X = X, test_mean = func, theta_init = c(1,1), verbose = FALSE,
                         covariance = list(Sigma = Sig), override = list(mcsim_stats = dfrfunc_1[["mcsim_stats"]]))

identical(dfrfunc_1[["epsp"]], dfrfunc_2[["epsp"]])# TRUE
identical(dfrfunc_1[["epsp"]], dfrfunc_3[["epsp"]])# TRUE
identical(dfrfunc_1[["epsp"]], dfrfunc_4[["epsp"]])# TRUE
identical(dfrfunc_1[["epsp"]], dfrfunc_5[["epsp"]])# TRUE
identical(dfrfunc_1[["p"]], dfrfunc_5[["p"]])# TRUE





my_res_order <- sample(1:n)
my_theta_hat <- c(7,13)
my_r <- dfrfunc_1[["r"]][sample(1:n),]
my_mcsim_stats <- list(KS = dfrfunc_1[["mcsim_stats"]][["KS"]]^2,
                       CvM = dfrfunc_1[["mcsim_stats"]][["CvM"]]^2)
set.seed(20240319)
dfrfunc_5 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig),
                         theta_init = c(1,1), verbose = FALSE,
                         override = list(res_order = my_res_order))
set.seed(20240319)
dfrfunc_6 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig),
                         theta_init = c(1,1), verbose = FALSE,
                         override = list(theta_hat = my_theta_hat))
set.seed(20240319)
dfrfunc_7 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig),
                         theta_init = c(1,1), verbose = FALSE,
                         override = list(r = my_r))
set.seed(20240319)
dfrfunc_8 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig),
                         theta_init = c(1,1), verbose = FALSE,
                         override = list(mcsim_stats = my_mcsim_stats))

# Verify that specified overrides are correct.
identical(dfrfunc_1[["res_order"]], dfrfunc_5[["res_order"]])# FALSE
identical(dfrfunc_1[["theta_hat"]], dfrfunc_6[["theta_hat"]])# FALSE
identical(dfrfunc_1[["r"]], dfrfunc_7[["r"]])# FALSE
identical(dfrfunc_1[["mcsim_stats"]], dfrfunc_8[["mcsim_stats"]])# FALSE
identical(my_res_order, dfrfunc_5[["res_order"]])# TRUE
identical(my_theta_hat, dfrfunc_6[["theta_hat"]])# TRUE
identical(my_r, dfrfunc_7[["r"]])# TRUE
identical(my_mcsim_stats, dfrfunc_8[["mcsim_stats"]])# TRUE

# Verify that other things changed or not, as appropriate.
identical(dfrfunc_1[["theta_hat"]], dfrfunc_5[["theta_hat"]])# TRUE
identical(dfrfunc_1[["optimization_output"]], dfrfunc_5[["optimization_output"]])# TRUE
identical(dfrfunc_1[["fitted_values"]], dfrfunc_5[["fitted_values"]])# TRUE
identical(dfrfunc_1[["J"]], dfrfunc_5[["J"]])# TRUE
identical(dfrfunc_1[["r"]], dfrfunc_5[["r"]])# FALSE
identical(dfrfunc_1[["r_tilde"]], dfrfunc_5[["r_tilde"]])# FALSE
identical(dfrfunc_1[["mu"]], dfrfunc_5[["mu"]])# TRUE
identical(dfrfunc_1[["residuals"]][["raw"]], dfrfunc_5[["residuals"]][["raw"]])# TRUE
identical(dfrfunc_1[["residuals"]][["sphered"]], dfrfunc_5[["residuals"]][["sphered"]])# TRUE
identical(dfrfunc_1[["residuals"]][["transformed"]], dfrfunc_5[["residuals"]][["transformed"]])# FALSE
identical(dfrfunc_1[["epsp"]], dfrfunc_5[["epsp"]])# FALSE
identical(dfrfunc_1[["observed_stats"]], dfrfunc_5[["observed_stats"]])# FALSE
identical(dfrfunc_1[["mcsim_stats"]], dfrfunc_5[["mcsim_stats"]])# FALSE
identical(dfrfunc_1[["p"]], dfrfunc_5[["p"]])# FALSE

identical(dfrfunc_1[["theta_hat"]], dfrfunc_6[["theta_hat"]])# FALSE
identical(dfrfunc_1[["optimization_output"]], dfrfunc_6[["optimization_output"]])# FALSE
identical(dfrfunc_1[["fitted_values"]], dfrfunc_6[["fitted_values"]])# FALSE
identical(dfrfunc_1[["J"]], dfrfunc_6[["J"]])# FALSE
identical(dfrfunc_1[["r"]], dfrfunc_6[["r"]])# TRUE
identical(dfrfunc_1[["r_tilde"]], dfrfunc_6[["r_tilde"]])# FALSE
identical(dfrfunc_1[["mu"]], dfrfunc_6[["mu"]])# FALSE
identical(dfrfunc_1[["residuals"]][["raw"]], dfrfunc_6[["residuals"]][["raw"]])# FALSE
identical(dfrfunc_1[["residuals"]][["sphered"]], dfrfunc_6[["residuals"]][["sphered"]])# FALSE
identical(dfrfunc_1[["residuals"]][["transformed"]], dfrfunc_6[["residuals"]][["transformed"]])# FALSE
identical(dfrfunc_1[["epsp"]], dfrfunc_6[["epsp"]])# FALSE
identical(dfrfunc_1[["observed_stats"]], dfrfunc_6[["observed_stats"]])# FALSE
identical(dfrfunc_1[["mcsim_stats"]], dfrfunc_6[["mcsim_stats"]])# TRUE
identical(dfrfunc_1[["p"]], dfrfunc_6[["p"]])# FALSE

identical(dfrfunc_1[["theta_hat"]], dfrfunc_7[["theta_hat"]])# TRUE
identical(dfrfunc_1[["optimization_output"]], dfrfunc_7[["optimization_output"]])# TRUE
identical(dfrfunc_1[["fitted_values"]], dfrfunc_7[["fitted_values"]])# TRUE
identical(dfrfunc_1[["J"]], dfrfunc_7[["J"]])# TRUE
identical(dfrfunc_1[["r"]], dfrfunc_7[["r"]])# FALSE
identical(dfrfunc_1[["r_tilde"]], dfrfunc_7[["r_tilde"]])# FALSE
identical(dfrfunc_1[["mu"]], dfrfunc_7[["mu"]])# TRUE
identical(dfrfunc_1[["residuals"]][["raw"]], dfrfunc_7[["residuals"]][["raw"]])# TRUE
identical(dfrfunc_1[["residuals"]][["sphered"]], dfrfunc_7[["residuals"]][["sphered"]])# TRUE
identical(dfrfunc_1[["residuals"]][["transformed"]], dfrfunc_7[["residuals"]][["transformed"]])# FALSE
identical(dfrfunc_1[["epsp"]], dfrfunc_7[["epsp"]])# FALSE
identical(dfrfunc_1[["observed_stats"]], dfrfunc_7[["observed_stats"]])# FALSE
identical(dfrfunc_1[["mcsim_stats"]], dfrfunc_7[["mcsim_stats"]])# FALSE
identical(dfrfunc_1[["p"]], dfrfunc_7[["p"]])# FALSE

identical(dfrfunc_1[["theta_hat"]], dfrfunc_8[["theta_hat"]])# TRUE
identical(dfrfunc_1[["optimization_output"]], dfrfunc_8[["optimization_output"]])# TRUE
identical(dfrfunc_1[["fitted_values"]], dfrfunc_8[["fitted_values"]])# TRUE
identical(dfrfunc_1[["J"]], dfrfunc_8[["J"]])# TRUE
identical(dfrfunc_1[["r"]], dfrfunc_8[["r"]])# TRUE
identical(dfrfunc_1[["r_tilde"]], dfrfunc_8[["r_tilde"]])# TRUE
identical(dfrfunc_1[["mu"]], dfrfunc_8[["mu"]])# TRUE
identical(dfrfunc_1[["residuals"]][["raw"]], dfrfunc_8[["residuals"]][["raw"]])# TRUE
identical(dfrfunc_1[["residuals"]][["sphered"]], dfrfunc_8[["residuals"]][["sphered"]])# TRUE
identical(dfrfunc_1[["residuals"]][["transformed"]], dfrfunc_8[["residuals"]][["transformed"]])# TRUE
identical(dfrfunc_1[["epsp"]], dfrfunc_8[["epsp"]])# TRUE
identical(dfrfunc_1[["observed_stats"]], dfrfunc_8[["observed_stats"]])# TRUE
identical(dfrfunc_1[["mcsim_stats"]], dfrfunc_8[["mcsim_stats"]])# FALSE
identical(dfrfunc_1[["p"]], dfrfunc_8[["p"]])# FALSE
