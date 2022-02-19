## Here I will define a simple function to sample from a
# multivariate gaussian distribution
library(testit)


# Random variable from the multivariate normal distribution
# Sigma is the covariance matrix
# n is the number of observations to sample
# mean_vector is the mean vector

# First we compute the cholesky factorization of the covariance
# matrix and then we take an affine extension of
# A U ~ N(0, 1)
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

# Let H be a square matrix
symmetrize <- function(H) {
    (H + t(H)) / 2
}


# Sample from the gaussian orthogonal ensemble
goe <- function(n) {
    # First generate n * n variables from a Normal random distribution
    mat <- matrix(rnorm(n * n), n, n)
    symmetrize(mat)
}

# Generate a symmetric positive definite matrix by multiplying two matrices from the
# GOE
rposdef <- function(m) {
    g <- goe(m)
    t(g) %*% g
}

# Sample from the multivariate normal distribution using a covariance matrix Sigma drawn
# from the Gaussian Orthogonal Ensemble, using m random variables. The mean_vector shall be 0
rgoenorm <- function(n, m) {

    sig <- rposdef(m)
    list(matrix = rmvnorm(n, sig, rep(0, m)), sigma = sig)
}
