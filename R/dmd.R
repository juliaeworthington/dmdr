#' Dynamic Mode Decomposition (DMD)
#'
#' Computes the dynamic mode decomposition of the data.
#' @import pracma
#' @param x matrix of raw data snapshots
#' @param y matrix of output data snapshots (optional)
#' @param svd.rank; int rank of SVD
#' @return dmd.obj; list containing lambda (vector of DMD eigenvalues), Phi (matrix of DMD modes) and b (vector of mode amplitudes)
#' @export

dmd = function(x, y=NULL, svd.rank) {
  # svd.rank must be within bounds of function
  if (svd.rank > nrow(x)) {warning("SVD rank must be less than the number of rows in the data matrix.")}
  else if (svd.rank > ncol(x)) {warning("SVD rank must be less than the number of columns in the data matrix.")}

  # convert data frame into matrix
  x = as.matrix(x); nt = ncol(x)

  # compute snapshot matrices if no output given
  if (is.null(y)) {X = x[,1:(nt-1)] # X in the DMD paper
  Y = x[,2:nt] # X' in the DMD paper
  } else {X = as.matrix(x); Y = as.matrix(y)}
  if (size(X)[1] != size(Y)[1]) {warning("Input and output matrices must be equal in dimension.")}
  else if (size(X)[2] != size(Y)[2]) {warning("Input and output matrices must be equal in dimension.")}

  # compute the svd.rank rank SVD
  svd = svd(X, nu=svd.rank, nv=svd.rank); d=svd$d; U = svd$u; V = svd$v

  # form A tilde = Y %*% V %*% ep.inverse %*% Conj(t(U))
  d.trunc = d[1:svd.rank]; ep.inverse = diag(1/d.trunc); A.tilde = Conj(t(U)) %*% Y %*% V %*% ep.inverse

  # eigendecomposition of A tilde
  eig = eigen(A.tilde); W = eig$vectors; lambda = eig$values

  # compute Phi (DMD modes) and b (mode amplitudes)
  Phi = Y %*% V %*% ep.inverse %*% W; b = as.vector(pinv(Phi) %*% X[,2])

  # return lambda, Phi, and b
  dmd.obj = list(lambda=lambda, Phi=Phi, b=b, nt=nt); return(dmd.obj)
}


