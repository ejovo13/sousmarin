# Covariance generalizes the notion of variance to multiple dimensions

x <- rnorm(1000)
y <- rnorm(1000, mean = 2, sd = 3.3)

A <- matrix(runif(100), ncol = 20, nrow = 5)
B <- matrix(runif(20), ncol = 5, nrow = 4)

# To create the Covariance matrix, the first thing that I want to do is create a
# data frame or a matrix

xy <- c(x, y)
# m <- matrix(xy, nrow = 10, ncol = 2)
# print(m)

ones <- function(mat) {
    # nc <- ncol(mat)
    nr <- nrow(mat)
    matrix(1.0, ncol = nr, nrow = nr)
}



cov_mat <- function(x, y) {

    if (length(x) != length(y)) stop("x and y are different lengths")

    n <- length(x)
    m <- matrix(c(x, y), nrow = n, ncol = 2)

    os <- ones(m)

    a <- m - (os %*% m) / n

    t(a) %*% a
}


# cov_mat <- function(df) {

# }


