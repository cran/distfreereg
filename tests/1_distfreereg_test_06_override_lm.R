library(distfreereg)
set.seed(20240319)
n <- 1e2
func <- function(x, theta) theta[1] + theta[2]*x
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
theta <- c(2,5)
X <- matrix(rexp(n, rate = 1))
Y <- distfreereg:::f2ftheta(f = func, X)(theta) +
  as.vector(distfreereg:::rmvnorm(n = n, reps = 1, mean = rep(0,n), SqrtSigma = distfreereg:::matsqrt(Sig)))

data <- data.frame(a = X, b = Y)
m <- lm(b ~ a, data = data)

set.seed(20240319)
dfrm_1 <- distfreereg(test_mean = m, verbose = FALSE)
set.seed(20240319)
dfrm_2 <- distfreereg(test_mean = m, verbose = FALSE,
                      override = list(res_order = dfrm_1[["res_order"]]))
set.seed(20240319)
dfrm_4 <- distfreereg(test_mean = m, verbose = FALSE,
                      override = list(r = dfrm_1[["r"]]))
set.seed(20240319)
dfrm_5 <- distfreereg(test_mean = m, verbose = FALSE,
                      override = list(mcsim_stats = dfrm_1[["mcsim_stats"]]))


identical(dfrm_1[["epsp"]], dfrm_2[["epsp"]])# TRUE
identical(dfrm_1[["epsp"]], dfrm_4[["epsp"]])# TRUE
identical(dfrm_1[["epsp"]], dfrm_5[["epsp"]])# TRUE
identical(dfrm_1[["p"]], dfrm_5[["p"]])# TRUE






my_res_order <- sample(1:n)
my_theta_hat <- c(7,13)
my_r <- dfrm_1[["r"]][sample(1:n),]
my_mcsim_stats <- list(KS = dfrm_1[["mcsim_stats"]][["KS"]]^2,
                       CvM = dfrm_1[["mcsim_stats"]][["CvM"]]^2)
set.seed(20240319)
dfrm_5 <- distfreereg(test_mean = m, verbose = FALSE,
                      override = list(res_order = my_res_order))
set.seed(20240319)
dfrm_7 <- distfreereg(test_mean = m, verbose = FALSE,
                      override = list(r = my_r))
set.seed(20240319)
dfrm_8 <- distfreereg(test_mean = m, verbose = FALSE,
                      override = list(mcsim_stats = my_mcsim_stats))

# Verify that specified overrides are correct.
identical(dfrm_1[["res_order"]], dfrm_5[["res_order"]])# FALSE
identical(dfrm_1[["r"]], dfrm_7[["r"]])# FALSE
identical(dfrm_1[["mcsim_stats"]], dfrm_8[["mcsim_stats"]])# FALSE
identical(my_res_order, dfrm_5[["res_order"]])# TRUE
identical(my_r, dfrm_7[["r"]])# TRUE
identical(my_mcsim_stats, dfrm_8[["mcsim_stats"]])# TRUE

# Verify that other things changed or not, as appropriate.
identical(dfrm_1[["theta_hat"]], dfrm_5[["theta_hat"]])# TRUE
identical(dfrm_1[["optimization_output"]], dfrm_5[["optimization_output"]])# TRUE
identical(dfrm_1[["fitted_values"]], dfrm_5[["fitted_values"]])# TRUE
identical(dfrm_1[["J"]], dfrm_5[["J"]])# TRUE
identical(dfrm_1[["r"]], dfrm_5[["r"]])# FALSE
identical(dfrm_1[["r_tilde"]], dfrm_5[["r_tilde"]])# FALSE
identical(dfrm_1[["mu"]], dfrm_5[["mu"]])# TRUE
identical(dfrm_1[["residuals"]][["raw"]], dfrm_5[["residuals"]][["raw"]])# TRUE
identical(dfrm_1[["residuals"]][["sphered"]], dfrm_5[["residuals"]][["sphered"]])# TRUE
identical(dfrm_1[["residuals"]][["transformed"]], dfrm_5[["residuals"]][["transformed"]])# FALSE
identical(dfrm_1[["epsp"]], dfrm_5[["epsp"]])# FALSE
identical(dfrm_1[["observed_stats"]], dfrm_5[["observed_stats"]])# FALSE
identical(dfrm_1[["mcsim_stats"]], dfrm_5[["mcsim_stats"]])# FALSE
identical(dfrm_1[["p"]], dfrm_5[["p"]])# FALSE

identical(dfrm_1[["theta_hat"]], dfrm_7[["theta_hat"]])# TRUE
identical(dfrm_1[["optimization_output"]], dfrm_7[["optimization_output"]])# TRUE
identical(dfrm_1[["fitted_values"]], dfrm_7[["fitted_values"]])# TRUE
identical(dfrm_1[["J"]], dfrm_7[["J"]])# TRUE
identical(dfrm_1[["r"]], dfrm_7[["r"]])# FALSE
identical(dfrm_1[["r_tilde"]], dfrm_7[["r_tilde"]])# FALSE
identical(dfrm_1[["mu"]], dfrm_7[["mu"]])# TRUE
identical(dfrm_1[["residuals"]][["raw"]], dfrm_7[["residuals"]][["raw"]])# TRUE
identical(dfrm_1[["residuals"]][["sphered"]], dfrm_7[["residuals"]][["sphered"]])# TRUE
identical(dfrm_1[["residuals"]][["transformed"]], dfrm_7[["residuals"]][["transformed"]])# FALSE
identical(dfrm_1[["epsp"]], dfrm_7[["epsp"]])# FALSE
identical(dfrm_1[["observed_stats"]], dfrm_7[["observed_stats"]])# FALSE
identical(dfrm_1[["mcsim_stats"]], dfrm_7[["mcsim_stats"]])# FALSE
identical(dfrm_1[["p"]], dfrm_7[["p"]])# FALSE

identical(dfrm_1[["theta_hat"]], dfrm_8[["theta_hat"]])# TRUE
identical(dfrm_1[["optimization_output"]], dfrm_8[["optimization_output"]])# TRUE
identical(dfrm_1[["fitted_values"]], dfrm_8[["fitted_values"]])# TRUE
identical(dfrm_1[["J"]], dfrm_8[["J"]])# TRUE
identical(dfrm_1[["r"]], dfrm_8[["r"]])# TRUE
identical(dfrm_1[["r_tilde"]], dfrm_8[["r_tilde"]])# TRUE
identical(dfrm_1[["mu"]], dfrm_8[["mu"]])# TRUE
identical(dfrm_1[["residuals"]][["raw"]], dfrm_8[["residuals"]][["raw"]])# TRUE
identical(dfrm_1[["residuals"]][["sphered"]], dfrm_8[["residuals"]][["sphered"]])# TRUE
identical(dfrm_1[["residuals"]][["transformed"]], dfrm_8[["residuals"]][["transformed"]])# TRUE
identical(dfrm_1[["epsp"]], dfrm_8[["epsp"]])# TRUE
identical(dfrm_1[["observed_stats"]], dfrm_8[["observed_stats"]])# TRUE
identical(dfrm_1[["mcsim_stats"]], dfrm_8[["mcsim_stats"]])# FALSE
identical(dfrm_1[["p"]], dfrm_8[["p"]])# FALSE
