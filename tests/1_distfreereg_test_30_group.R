library(distfreereg)

test_mean <- function(x, theta) (theta[1] + theta[2]*x[1] + theta[3]*x[1]^2) * x[2]^(2/3)
theta <- c(5,2,4)
X <- cbind(rep(1:8, 9), rep(30 + 20*(0:8), each = 8))
X <- X[rep(1:72, times = 2),]
colnames(X) <- c("a", "b")

set.seed(20240416)
cdfr_0 <- compare(true_mean = test_mean, true_covariance = list(Sigma = 1),
                  covariance = list(Sigma = 1),
                  theta = theta, test_mean = test_mean, theta_init = c(1,1,1),
                  true_X = X, X = X, ordering = list(2,1), group = TRUE,
                  reps = 10, B = 10)
cdfr_0

set.seed(20240416)
cdfr_1 <- compare(true_mean = test_mean, true_covariance = list(Sigma = 1),
                  covariance = list(Sigma = 1),
                  theta = theta, test_mean = test_mean, theta_init = c(1,1,1),
                  true_X = X, X = X, ordering = list("b", "a"), group = TRUE,
                  reps = 10, B = 10)

identical(cdfr_0[["observed_stats"]], cdfr_1[["observed_stats"]])# TRUE
identical(cdfr_0[["mcsim_stats"]], cdfr_1[["mcsim_stats"]])# TRUE


set.seed(20240416)
cdfr_2 <- compare(true_mean = test_mean, true_covariance = list(Sigma = 1),
                  covariance = list(Sigma = 1),
                  theta = theta, test_mean = test_mean, theta_init = c(1,1,1),
                  true_X = X, X = X, ordering = list(1,2), group = TRUE,
                  reps = 10, B = 10)

set.seed(20240416)
cdfr_3 <- compare(true_mean = test_mean, true_covariance = list(Sigma = 1),
                  covariance = list(Sigma = 1),
                  theta = theta, test_mean = test_mean, theta_init = c(1,1,1),
                  true_X = X, X = X, ordering = list("a", "b"), group = TRUE,
                  reps = 10, B = 10)

set.seed(20240416)
cdfr_4 <- compare(true_mean = test_mean, true_covariance = list(Sigma = 1),
                  covariance = list(Sigma = 1),
                  theta = theta, test_mean = test_mean, theta_init = c(1,1,1),
                  true_X = X, X = X, ordering = "natural", group = TRUE,
                  reps = 10, B = 10)

identical(cdfr_2[["observed_stats"]], cdfr_3[["observed_stats"]])# TRUE
identical(cdfr_2[["observed_stats"]], cdfr_4[["observed_stats"]])# TRUE
identical(cdfr_2[["mcsim_stats"]], cdfr_3[["mcsim_stats"]])# TRUE
identical(cdfr_2[["mcsim_stats"]], cdfr_4[["mcsim_stats"]])# TRUE
