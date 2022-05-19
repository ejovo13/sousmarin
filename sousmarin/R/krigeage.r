library(pracma)
library(purrr)

n <- 5 # start with 5 x 5 grid

#' @export
ind_to_coord <- function(index, n) {
    c(floor((index - 1) / n) + 1, mod(index - 1, n) + 1)
}

#' @export
coord_to_dist <- function(pos1, pos2) {
    sqrt((pos1[1] - pos2[1]) ** 2 + (pos1[2] - pos2[2]) ** 2)
}

#' @export
cov_model <- function(h, a = sqrt(8)) {
    exp(-abs(h) / a)
}

# i is the row of the Sigma matrix
#' @export
cov_get_row <- function(i, n, a = sqrt(8)) {

    len_out <- (n * n) - i + 1
    x <- rep(0, len_out)
    c1 <- ind_to_coord(i, n)

    for (j in 1:len_out) {
        x[j] <- cov_model(coord_to_dist(c1, ind_to_coord(j + i - 1, n)), a)
    }

    x
}

#' @export
get_cov_matrix <- function(n, a = 2) {

    n2 <- n * n
    sigma <- matrix(rep(0, n2 * n2), nrow = n2)

    for (i in 1:n2) {
        sigma[i, i:n2] <- cov_get_row(i, n, a)
    }

    sigma + t(sigma) - diag(n * n)
}