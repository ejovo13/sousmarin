## Here I will define a simple function to sample from a
# multivariate gaussian distribution
library(testit)


#' Random variable from the multivariate normal distribution
#' Sigma is the covariance matrix
#' n is the number of observations to sample
#' mean_vector is the mean vector
#'
#' First we compute the cholesky factorization of the covariance
#' matrix and then we take an affine extension of
#' A U ~ N(0, 1)
#' @param n The number of obervations to sample
#' @param sigma The covariance matrix
#' @param mean_vector The mean vector
#' @return n observations of the p-multivariate X_p ~ N_p(mean_vector, sigma) stored in a n x p matrix
#' @export
#' @examples
#' rmvnorm(10000, matrix(c(1, 0.5, 0.5, 1), 2, 2), rep(0, 2))
rmvnorm <- function(n, sigma, mean_vector) {

    # The size of sigma and the mean_vector determine the number
    # of variables m.

    # So let's start of with sigma
    m <- ncol(sigma)

    # Assert that sigma is square
    assert("Sigma is not square", ncol(sigma) == nrow(sigma))
    # Now assert that the mean_vector has the same length as m
    assert("Mean_vector is not the proper length", length(mean_vector) == m)

    # Take the Cholesky factorization of the Covariance matrix Sigma
    L <- chol(sigma)

    # Replicate the mean_vector n times
    mean_matrix <- matrix(mean_vector, nrow <- m, ncol <- n)

    # Now generate n * m random variables, storing them in an m x n matrix
    u <- matrix(rnorm(n * m), nrow <- m, ncol <- n)

    # Apply L to u and add the mean vector
    # Z = m + Lu
    t(mean_matrix + (L %*% u))
}

#' Make a matrix symmetric
#'
#' This routine is used to create a matrix variate that is a part of the Gaussian Orthogonal Ensemble
#' @param H The matrix to symmetrize
#' @return The symmetric matrix (H + H')/2
#' @export
#' @examples
#' symmetrize(matrix(c(1, 2, 3, 4), 2, 2)) => matrix(c(1, 2.5, 2.5, 4))
symmetrize <- function(H) {
    (H + t(H)) / 2
}


#' Sample from the gaussian orthogonal ensemble
#'
#' We first generate a matrix whose elements are independent variables taken from a univariate
#' standard normal distribution. We then symmetrize this matrix and the result is from
#' the GOE.
#' @param n Size of the M(n x n) matrix to sample
#' @return An n x n matrix sampled from the GOE
#' @export
#' @examples
#' g  <- goe(10)
#' g1 <- goe(100)
goe <- function(n) {
    # First generate n * n variables from a Normal random distribution
    mat <- matrix(rnorm(n * n), n, n)
    symmetrize(mat)
}

#' Generate a symmetric positive definite matrix by multiplying a matrix sampled from the goe with its transpose.
#'
#' Any invertible matrix multiplied by its transpose is positive definite. Therefore, we sample a matrix from the goe
#' and then we multiply it with its transpose to get a matrix that is positive definite. Since singular matrices generated
#' randomly are extremely rare, there is a high probability that the function returned is indeed positive definite.
#' @export
rposdef <- function(m) {
    g <- goe(m)
    t(g) %*% g
}

#' Sample from the multivariate normal distribution using a covariance matrix Sigma drawn
#' from the Gaussian Orthogonal Ensemble, using m random variables. The mean_vector shall be 0
#' @export
rgoenorm <- function(n, m) {

    sig <- rposdef(m)
    list(matrix = rmvnorm(n, sig, rep(0, m)), sigma = sig)
}
