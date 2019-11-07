# aborted attempt to port BIRSVD (https://www.mat.univie.ac.at/~neum/software/birsvd/)
# status: breaks if any weights are zero (i.e., NA obs), 
#         unclear how to tune penalization
#   direct port of their BIRSVD routine, 
#   should probably have tried to reimplement their BIRSVD2 ?
#   (more efficient, more direct LS solver steps)

library(Matrix)
n_iter <- 10

n <- 10 # N
p <- 200 # M
rnk <- 5 #min(n, p)

# uses nobs as columns!!
#x <- seq(-1, 1, l = p)
x <- c(0, sort(runif(p - 2)), 1)
set.seed(121344)
Af <- tf_rgp(n, arg = x, nugget = 2/500)
# tfb_fpc(Af) %>% attr(., "basis") %>% environment(.) %$% efunctions %>% plot
A <- t(as.matrix(Af)) #matrix(runif(n*p), ncol = n)

#na_mask <- matrix(rbinom(n*p, size= 1, p = .995), ncol = n, nrow = p)
#W <- na_mask
#W <- matrix(1, ncol = n, nrow = p)
W <- matrix(c(0, diff(x)/2), ncol = n, nrow = p)

A_w <- A * W

# initialize U
U <-  qr.Q(qr(matrix(runif(p*rnk), ncol = rnk))) #svd(A_w)$u[, 1:rnk]

U_true <- svd(A)$u[, 1:rnk]
V_true <- svd(A)$v[, 1:rnk]
S_true <- svd(A)$d[1:rnk]
A_true <- U_true %*% diag(S_true) %*% t(V_true)

pen_U <- 0 * diag(n)
pen_V <- .01 * crossprod(diff(diff(diag(p))))

for (i in 1:n_iter) {
  # Computing the right approximants from the left approximants.
  R <- as.vector(t(U) %*% A_w)
  L <- do.call(bdiag, map(1:n, ~ crossprod(U, W[,.x] * U))) +
    Diagonal(rnk) %x% pen_U
  #L_L <- chol(L)
  #Y <- backsolve(L_L, backsolve(t(L_L), R))
  Y <- solve(L, R) 
  Y <- matrix(Y, rnk, n)
  svd_r <- svd(Y, nu = rnk, nv = rnk)
  S <- svd_r$d
  V <- svd_r$v
  
  #fix signs:
  for (r in 1:rnk) {
    i <- which.max(abs(svd_r$u[,r])) #svd_r$u is diagonal with 1/-1 entries!?
    V[, r] <- sign(svd_r$u[i,r]) * V[, r]
  }
  
  
  # Computing the left approximants from the right approximants.
  R <- as.vector(t(A_w %*% t(t(V)/S)))
  L <- do.call(bdiag, map(1:p, ~ crossprod(V, W[.x,] * V))) +
    Diagonal(rnk) %x% pen_V
  #lu_L <- lu(L)
  X <- solve(L, R) #backsolve(lu_L@U, backsolve(t(lu_L@L), R))
  qrX <- qr(t(matrix(X, rnk, p)))
  #orthogonalize & fix signs:
  U <- qr.Q(qrX, Dvec = sign(diag(qr.R(qrX))))

  A_approx <- U %*% diag(S) %*% t(V)
  mean(abs(A_approx - A))
  cat(mean(abs(A_approx - A_true)), "\n")
  layout(t(1:2)); matplot(A, type = "l"); matplot(A_approx, type = "l", ylim = range(A))
  layout(t(1:2)); matplot(U_true, type = "l"); matplot(U, type = "l")
  # layout(t(1:2)); matplot(V_true, type = "b"); matplot(V, type = "b")
  # 
  # all_equal(S, S_true)
}


