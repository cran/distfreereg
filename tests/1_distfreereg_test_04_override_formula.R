library(distfreereg)
set.seed(20240319)
n <- 1e2
func <- function(x, theta) theta[1] + theta[2]*x
Sig <- diag(rexp(n))
theta <- c(2,5)
X <- matrix(rexp(n, rate = 1))
Y <- distfreereg:::f2ftheta(f = func, X)(theta) +
  as.vector(distfreereg:::rmvnorm(n = n, reps = 1, mean = rep(0,n), SqrtSigma = distfreereg:::matsqrt(Sig)))

data <- data.frame(a = X, b = Y)


# First, method = "lm".

set.seed(20240319)
dfrform_lm_1 <- distfreereg(test_mean = b ~ a, data = data, covariance = list(Sigma = Sig),
                            verbose = FALSE)
set.seed(20240319)
dfrform_lm_2 <- distfreereg(test_mean = b ~ a, data = data, covariance = list(Sigma = Sig),
                            verbose = FALSE,
                            override = list(res_order = dfrform_lm_1[["res_order"]]))
set.seed(20240319)
dfrform_lm_4 <- distfreereg(test_mean = b ~ a, data = data, covariance = list(Sigma = Sig),
                            verbose = FALSE,
                            override = list(r = dfrform_lm_1[["r"]]))
set.seed(20240319)
dfrform_lm_5 <- distfreereg(test_mean = b ~ a, data = data, covariance = list(Sigma = Sig),
                            verbose = FALSE,
                            override = list(mcsim_stats = dfrform_lm_1[["mcsim_stats"]]))


identical(dfrform_lm_1[["epsp"]], dfrform_lm_2[["epsp"]])# TRUE
identical(dfrform_lm_1[["epsp"]], dfrform_lm_4[["epsp"]])# TRUE
identical(dfrform_lm_1[["epsp"]], dfrform_lm_5[["epsp"]])# TRUE
identical(dfrform_lm_1[["p"]], dfrform_lm_5[["p"]])# TRUE






my_res_order <- sample(1:n)
my_theta_hat <- c(7,13)
my_r <- dfrform_lm_1[["r"]][sample(1:n),]
my_mcsim_stats <- list(KS = dfrform_lm_1[["mcsim_stats"]][["KS"]]^2,
                       CvM = dfrform_lm_1[["mcsim_stats"]][["CvM"]]^2)
set.seed(20240319)
dfrform_lm_5 <- distfreereg(test_mean = b ~ a, data = data, covariance = list(Sigma = Sig),
                            verbose = FALSE,
                            override = list(res_order = my_res_order))
set.seed(20240319)
dfrform_lm_7 <- distfreereg(test_mean = b ~ a, data = data, covariance = list(Sigma = Sig),
                            verbose = FALSE,
                            override = list(r = my_r))
set.seed(20240319)
dfrform_lm_8 <- distfreereg(test_mean = b ~ a, data = data, covariance = list(Sigma = Sig),
                            verbose = FALSE,
                            override = list(mcsim_stats = my_mcsim_stats))

# Verify that specified overrides are correct.
identical(dfrform_lm_1[["res_order"]], dfrform_lm_5[["res_order"]])# FALSE
identical(dfrform_lm_1[["r"]], dfrform_lm_7[["r"]])# FALSE
identical(dfrform_lm_1[["mcsim_stats"]], dfrform_lm_8[["mcsim_stats"]])# FALSE
identical(my_res_order, dfrform_lm_5[["res_order"]])# TRUE
identical(my_r, dfrform_lm_7[["r"]])# TRUE
identical(my_mcsim_stats, dfrform_lm_8[["mcsim_stats"]])# TRUE

# Verify that other things changed or not, as appropriate.
identical(dfrform_lm_1[["theta_hat"]], dfrform_lm_5[["theta_hat"]])# TRUE
identical(dfrform_lm_1[["optimization_output"]], dfrform_lm_5[["optimization_output"]])# TRUE
identical(dfrform_lm_1[["fitted_values"]], dfrform_lm_5[["fitted_values"]])# TRUE
identical(dfrform_lm_1[["J"]], dfrform_lm_5[["J"]])# TRUE
identical(dfrform_lm_1[["r"]], dfrform_lm_5[["r"]])# FALSE
identical(dfrform_lm_1[["r_tilde"]], dfrform_lm_5[["r_tilde"]])# FALSE
identical(dfrform_lm_1[["mu"]], dfrform_lm_5[["mu"]])# TRUE
identical(dfrform_lm_1[["residuals"]][["raw"]], dfrform_lm_5[["residuals"]][["raw"]])# TRUE
identical(dfrform_lm_1[["residuals"]][["sphered"]], dfrform_lm_5[["residuals"]][["sphered"]])# TRUE
identical(dfrform_lm_1[["residuals"]][["transformed"]], dfrform_lm_5[["residuals"]][["transformed"]])# FALSE
identical(dfrform_lm_1[["epsp"]], dfrform_lm_5[["epsp"]])# FALSE
identical(dfrform_lm_1[["observed_stats"]], dfrform_lm_5[["observed_stats"]])# FALSE
identical(dfrform_lm_1[["mcsim_stats"]], dfrform_lm_5[["mcsim_stats"]])# FALSE
identical(dfrform_lm_1[["p"]], dfrform_lm_5[["p"]])# FALSE

identical(dfrform_lm_1[["theta_hat"]], dfrform_lm_7[["theta_hat"]])# TRUE
identical(dfrform_lm_1[["optimization_output"]], dfrform_lm_7[["optimization_output"]])# TRUE
identical(dfrform_lm_1[["fitted_values"]], dfrform_lm_7[["fitted_values"]])# TRUE
identical(dfrform_lm_1[["J"]], dfrform_lm_7[["J"]])# TRUE
identical(dfrform_lm_1[["r"]], dfrform_lm_7[["r"]])# FALSE
identical(dfrform_lm_1[["r_tilde"]], dfrform_lm_7[["r_tilde"]])# FALSE
identical(dfrform_lm_1[["mu"]], dfrform_lm_7[["mu"]])# TRUE
identical(dfrform_lm_1[["residuals"]][["raw"]], dfrform_lm_7[["residuals"]][["raw"]])# TRUE
identical(dfrform_lm_1[["residuals"]][["sphered"]], dfrform_lm_7[["residuals"]][["sphered"]])# TRUE
identical(dfrform_lm_1[["residuals"]][["transformed"]], dfrform_lm_7[["residuals"]][["transformed"]])# FALSE
identical(dfrform_lm_1[["epsp"]], dfrform_lm_7[["epsp"]])# FALSE
identical(dfrform_lm_1[["observed_stats"]], dfrform_lm_7[["observed_stats"]])# FALSE
identical(dfrform_lm_1[["mcsim_stats"]], dfrform_lm_7[["mcsim_stats"]])# FALSE
identical(dfrform_lm_1[["p"]], dfrform_lm_7[["p"]])# FALSE

identical(dfrform_lm_1[["theta_hat"]], dfrform_lm_8[["theta_hat"]])# TRUE
identical(dfrform_lm_1[["optimization_output"]], dfrform_lm_8[["optimization_output"]])# TRUE
identical(dfrform_lm_1[["fitted_values"]], dfrform_lm_8[["fitted_values"]])# TRUE
identical(dfrform_lm_1[["J"]], dfrform_lm_8[["J"]])# TRUE
identical(dfrform_lm_1[["r"]], dfrform_lm_8[["r"]])# TRUE
identical(dfrform_lm_1[["r_tilde"]], dfrform_lm_8[["r_tilde"]])# TRUE
identical(dfrform_lm_1[["mu"]], dfrform_lm_8[["mu"]])# TRUE
identical(dfrform_lm_1[["residuals"]][["raw"]], dfrform_lm_8[["residuals"]][["raw"]])# TRUE
identical(dfrform_lm_1[["residuals"]][["sphered"]], dfrform_lm_8[["residuals"]][["sphered"]])# TRUE
identical(dfrform_lm_1[["residuals"]][["transformed"]], dfrform_lm_8[["residuals"]][["transformed"]])# TRUE
identical(dfrform_lm_1[["epsp"]], dfrform_lm_8[["epsp"]])# TRUE
identical(dfrform_lm_1[["observed_stats"]], dfrform_lm_8[["observed_stats"]])# TRUE
identical(dfrform_lm_1[["mcsim_stats"]], dfrform_lm_8[["mcsim_stats"]])# FALSE
identical(dfrform_lm_1[["p"]], dfrform_lm_8[["p"]])# FALSE










# Second, method = "nls".

set.seed(20240319)
dfrform_nls_1 <- distfreereg(test_mean = b ~ f + g*a^h, data = data, covariance = list(Sigma = Sig),
                             verbose = FALSE, method = "nls")
set.seed(20240319)
dfrform_nls_2 <- distfreereg(test_mean = b ~ f + g*a^h, data = data, covariance = list(Sigma = Sig),
                             verbose = FALSE, method = "nls",
                             override = list(res_order = dfrform_nls_1[["res_order"]]))
set.seed(20240319)
tryCatch(dfrform_nls_3 <- distfreereg(test_mean = b ~ f + g*a^h, data = data, covariance = list(Sigma = Sig),
                                      verbose = FALSE, method = "nls",
                                      override = list(theta_hat = dfrform_nls_1[["theta_hat"]])),
         error = function(e) warning(e))
set.seed(20240319)
dfrform_nls_4 <- distfreereg(test_mean = b ~ f + g*a^h, data = data, covariance = list(Sigma = Sig),
                             verbose = FALSE, method = "nls",
                             override = list(r = dfrform_nls_1[["r"]]))
set.seed(20240319)
dfrform_nls_5 <- distfreereg(test_mean = b ~ f + g*a^h, data = data, covariance = list(Sigma = Sig),
                             verbose = FALSE, method = "nls",
                             override = list(mcsim_stats = dfrform_nls_1[["mcsim_stats"]]))


identical(dfrform_nls_1[["epsp"]], dfrform_nls_2[["epsp"]])# TRUE
identical(dfrform_nls_1[["epsp"]], dfrform_nls_4[["epsp"]])# TRUE
identical(dfrform_nls_1[["epsp"]], dfrform_nls_5[["epsp"]])# TRUE
identical(dfrform_nls_1[["p"]], dfrform_nls_5[["p"]])# TRUE






my_res_order <- sample(1:n)
my_theta_hat <- c(7,13)
my_r <- dfrform_nls_1[["r"]][sample(1:n),]
my_mcsim_stats <- list(KS = dfrform_nls_1[["mcsim_stats"]][["KS"]]^2,
                       CvM = dfrform_nls_1[["mcsim_stats"]][["CvM"]]^2)
set.seed(20240319)
dfrform_nls_5 <- distfreereg(test_mean = b ~ f + g*a^h, data = data, covariance = list(Sigma = Sig),
                             verbose = FALSE, method = "nls",
                             override = list(res_order = my_res_order))
set.seed(20240319)
tryCatch(dfrform_nls_6 <- distfreereg(test_mean = b ~ f + g*a^h, data = data, covariance = list(Sigma = Sig),
                                      verbose = FALSE, method = "nls",
                                      override = list(theta_hat = my_theta_hat)),
         error = function(e) e)
set.seed(20240319)
dfrform_nls_7 <- distfreereg(test_mean = b ~ f + g*a^h, data = data, covariance = list(Sigma = Sig),
                             verbose = FALSE, method = "nls",
                             override = list(r = my_r))
set.seed(20240319)
dfrform_nls_8 <- distfreereg(test_mean = b ~ f + g*a^h, data = data, covariance = list(Sigma = Sig),
                             verbose = FALSE, method = "nls",
                             override = list(mcsim_stats = my_mcsim_stats))

# Verify that specified overrides are correct.
identical(dfrform_nls_1[["res_order"]], dfrform_nls_5[["res_order"]])# FALSE
# identical(dfrform_nls_1[["theta_hat"]], dfrform_nls_6[["theta_hat"]])# FALSE
identical(dfrform_nls_1[["r"]], dfrform_nls_7[["r"]])# FALSE
identical(dfrform_nls_1[["mcsim_stats"]], dfrform_nls_8[["mcsim_stats"]])# FALSE
identical(my_res_order, dfrform_nls_5[["res_order"]])# TRUE
# identical(my_theta_hat, dfrform_nls_6[["theta_hat"]])# TRUE
identical(my_r, dfrform_nls_7[["r"]])# TRUE
identical(my_mcsim_stats, dfrform_nls_8[["mcsim_stats"]])# TRUE

# Verify that other things changed or not, as appropriate.
identical(dfrform_nls_1[["theta_hat"]], dfrform_nls_5[["theta_hat"]])# TRUE
identical(dfrform_nls_1[["fitted_values"]], dfrform_nls_5[["fitted_values"]])# TRUE
identical(dfrform_nls_1[["J"]], dfrform_nls_5[["J"]])# TRUE
identical(dfrform_nls_1[["r"]], dfrform_nls_5[["r"]])# FALSE
identical(dfrform_nls_1[["r_tilde"]], dfrform_nls_5[["r_tilde"]])# FALSE
identical(dfrform_nls_1[["mu"]], dfrform_nls_5[["mu"]])# TRUE
identical(dfrform_nls_1[["residuals"]][["raw"]], dfrform_nls_5[["residuals"]][["raw"]])# TRUE
identical(dfrform_nls_1[["residuals"]][["sphered"]], dfrform_nls_5[["residuals"]][["sphered"]])# TRUE
identical(dfrform_nls_1[["residuals"]][["transformed"]], dfrform_nls_5[["residuals"]][["transformed"]])# FALSE
identical(dfrform_nls_1[["epsp"]], dfrform_nls_5[["epsp"]])# FALSE
identical(dfrform_nls_1[["observed_stats"]], dfrform_nls_5[["observed_stats"]])# FALSE
identical(dfrform_nls_1[["mcsim_stats"]], dfrform_nls_5[["mcsim_stats"]])# FALSE
identical(dfrform_nls_1[["p"]], dfrform_nls_5[["p"]])# FALSE

# identical(dfrform_nls_1[["theta_hat"]], dfrform_nls_6[["theta_hat"]])# FALSE
# identical(dfrform_nls_1[["optimization_output"]], dfrform_nls_6[["optimization_output"]])# FALSE
# identical(dfrform_nls_1[["fitted_values"]], dfrform_nls_6[["fitted_values"]])# FALSE
# identical(dfrform_nls_1[["J"]], dfrform_nls_6[["J"]])# FALSE
# identical(dfrform_nls_1[["r"]], dfrform_nls_6[["r"]])# TRUE
# identical(dfrform_nls_1[["r_tilde"]], dfrform_nls_6[["r_tilde"]])# FALSE
# identical(dfrform_nls_1[["mu"]], dfrform_nls_6[["mu"]])# FALSE
# identical(dfrform_nls_1[["residuals"]][["raw"]], dfrform_nls_6[["residuals"]][["raw"]])# FALSE
# identical(dfrform_nls_1[["residuals"]][["sphered"]], dfrform_nls_6[["residuals"]][["sphered"]])# FALSE
# identical(dfrform_nls_1[["residuals"]][["transformed"]], dfrform_nls_6[["residuals"]][["transformed"]])# FALSE
# identical(dfrform_nls_1[["epsp"]], dfrform_nls_6[["epsp"]])# FALSE
# identical(dfrform_nls_1[["observed_stats"]], dfrform_nls_6[["observed_stats"]])# FALSE
# identical(dfrform_nls_1[["mcsim_stats"]], dfrform_nls_6[["mcsim_stats"]])# TRUE
# identical(dfrform_nls_1[["p"]], dfrform_nls_6[["p"]])# FALSE

identical(dfrform_nls_1[["theta_hat"]], dfrform_nls_7[["theta_hat"]])# TRUE
identical(dfrform_nls_1[["fitted_values"]], dfrform_nls_7[["fitted_values"]])# TRUE
identical(dfrform_nls_1[["J"]], dfrform_nls_7[["J"]])# TRUE
identical(dfrform_nls_1[["r"]], dfrform_nls_7[["r"]])# FALSE
identical(dfrform_nls_1[["r_tilde"]], dfrform_nls_7[["r_tilde"]])# FALSE
identical(dfrform_nls_1[["mu"]], dfrform_nls_7[["mu"]])# TRUE
identical(dfrform_nls_1[["residuals"]][["raw"]], dfrform_nls_7[["residuals"]][["raw"]])# TRUE
identical(dfrform_nls_1[["residuals"]][["sphered"]], dfrform_nls_7[["residuals"]][["sphered"]])# TRUE
identical(dfrform_nls_1[["residuals"]][["transformed"]], dfrform_nls_7[["residuals"]][["transformed"]])# FALSE
identical(dfrform_nls_1[["epsp"]], dfrform_nls_7[["epsp"]])# FALSE
identical(dfrform_nls_1[["observed_stats"]], dfrform_nls_7[["observed_stats"]])# FALSE
identical(dfrform_nls_1[["mcsim_stats"]], dfrform_nls_7[["mcsim_stats"]])# FALSE
identical(dfrform_nls_1[["p"]], dfrform_nls_7[["p"]])# FALSE

identical(dfrform_nls_1[["theta_hat"]], dfrform_nls_8[["theta_hat"]])# TRUE
identical(dfrform_nls_1[["fitted_values"]], dfrform_nls_8[["fitted_values"]])# TRUE
identical(dfrform_nls_1[["J"]], dfrform_nls_8[["J"]])# TRUE
identical(dfrform_nls_1[["r"]], dfrform_nls_8[["r"]])# TRUE
identical(dfrform_nls_1[["r_tilde"]], dfrform_nls_8[["r_tilde"]])# TRUE
identical(dfrform_nls_1[["mu"]], dfrform_nls_8[["mu"]])# TRUE
identical(dfrform_nls_1[["residuals"]][["raw"]], dfrform_nls_8[["residuals"]][["raw"]])# TRUE
identical(dfrform_nls_1[["residuals"]][["sphered"]], dfrform_nls_8[["residuals"]][["sphered"]])# TRUE
identical(dfrform_nls_1[["residuals"]][["transformed"]], dfrform_nls_8[["residuals"]][["transformed"]])# TRUE
identical(dfrform_nls_1[["epsp"]], dfrform_nls_8[["epsp"]])# TRUE
identical(dfrform_nls_1[["observed_stats"]], dfrform_nls_8[["observed_stats"]])# TRUE
identical(dfrform_nls_1[["mcsim_stats"]], dfrform_nls_8[["mcsim_stats"]])# FALSE
identical(dfrform_nls_1[["p"]], dfrform_nls_8[["p"]])# FALSE
