default_distfreereg_tol <- function(){
  list(solve_tol = .Machine[["double.eps"]],
       qr_tol = sqrt(.Machine[["double.eps"]]),
       orth_tol = sqrt(.Machine[["double.eps"]]),
       trans_tol = sqrt(.Machine[["double.eps"]]))
}