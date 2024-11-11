set.seed(20240220)

n <- 20

Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
P <- distfreereg:::matinv(Sig, tol = eval(as.list(solve.default)[["tol"]]))

all.equal(diag(1, n), Sig %*% P)# TRUE

Q <- distfreereg:::matsqrt(P)

all.equal(Q %*% Q, P)# TRUE
