# Simple implementation of the inverse transform method
# for the exponential distribution with rate parameter \lambda
# We implement the sampling methods rexp_inv.
rexp_inv <- function(n, lambda = 1) {
    # Generate n realizations of a uniform random variable n times
    (-1 / lambda) * log(runif(n, 0, 1))
}
