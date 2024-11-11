calc_mu <-
function(J, tol){
  stopifnot(is.matrix(J), is.numeric(tol), tol > 0)
  n <- nrow(J)
  nR <- crossprod(J)
  nR_sqrt <- tryCatch(matsqrt(mat = nR),
                      error = function(e) stop("Unable to calculate square root of J^t %*% J: ", e))
  nR_sqrtinv <- tryCatch(matinv(mat = nR_sqrt, tol = tol),
                     error = function(e) stop("Unable to invert square root of J^tJ: ", e))
  mu <- t(nR_sqrtinv %*% as.matrix(t(J)))
  if(!isTRUE(all.equal(crossprod(mu), diag(ncol(mu)), check.attributes = FALSE)))
    warning("mu^t %*% mu is not equal to the identity matrix")
  return(mu)
}
