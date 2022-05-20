library(pracma)
library(purrr)

library(Rcpp)
sourceCpp(file = "src/krigeage.cpp")

#' @export
cov_model_tent_fac <- function(a, sigma) {
    function(h) {
        cov_model_tent(h, a, sigma)
    }
}

#' @export
cov_model_sphere_fac <- function(a, m) {
    function(h) {
        cov_model_sphere(h, a, m)
    }
}

#' @export
cov_model_gauss_fac <- function(a, m) {
    function(h) {
        cov_model_gauss(h, a, m)
    }
}

#' @export
cov_mode_exp_fac <- function(a) {
    function(h) {
        cov_model_exp(h, a)
    }
}

#' @export
get_cov_row_lambda <- function(coords_list, i, n_obs, a = sqrt(8)) {

    len_out <- n_obs - i + 1
    x <- rep(0, len_out)
    c1 <- coords_list[[i]]

    for (j in 1:len_out) {
        c2 <- coords_list[[j + i - 1]]
        x[j] <- cov_model(coord_to_dist(c1, c2), a)
    }

    x
}

# Left hand side of system of equations
#' @export
get_cov_matrix_lambda <- function(coords_list, a = sqrt(8)) {

    n_obs <- length(coords_list)
    for (i in 1:n_obs) {
        sigma[i, i:n_obs] <- get_cov_row_lambda(coords_list, i, n_obs, a)
    }

    sigma + t(sigma) - diag(n_obs)
}


# Right hand side of the system of equations to solve for lambda
# takes in a single x value, returns a vector that is n_obs long
# x is a single index, grid is n x n.
#' @export
get_cov_obs <- function(coords_list, x, n, a = sqrt(8)) {

    n_obs <- length(coords_list)
    out <- rep(0, n_obs)
    cx <- ind_to_coord(x, n)

    for (i in 1:n_obs) {
        c2 <- coords_list[[i]]
        out[i] <- cov_model(coord_to_dist(cx, c2), a)
    }

    out
}

# Solve system of equations to get weights lambda
#' @export
get_lambda <- function(coords_list, x, n, a = sqrt(8)) {

    A <- get_cov_matrix_lambda(coords_list, a)
    b <- get_cov_obs(coords_list, x, n, a)

    solve(A, b)
}