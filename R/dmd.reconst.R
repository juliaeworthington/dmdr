#' DMD Reconstruction
#'
#' Reconstructs dynamic mode decomposition input data.
#' @param dmd.obj; list, output from dmd() function
#' @return X.recon; matrix, reconstructed approximation of X matrix
#' @export

dmd.reconst = function(dmd.obj) {
  # extracting data from dmd list
  lambda = dmd.obj$lambda; Phi = dmd.obj$Phi; b = dmd.obj$b; nt = dmd.obj$nt

  # constructing Lambda matrix of eigenvalues raised to powers (vandermonde matrix with powers +1)
  Lambda = outer(lambda, seq(1, nt), `^`)

  # X_recon = Phi * diag(b) * Lambda
  X.recon = Phi %*% diag(b) %*% Lambda; return(X.recon)
}
