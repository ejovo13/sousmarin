library(pracma)
library(purrr)
library(testit)

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
        x[j] <- cov_model_exp(coord_to_dist(c1, c2), a)
    }

    x
}

# Left hand side of system of equations
#' @export
get_cov_matrix_lambda <- function(coords_list, a = sqrt(8)) {

    n_obs <- length(coords_list)
    sigma <- matrix(rep(0, n_obs * n_obs), nrow = n_obs)

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
        out[i] <- cov_model_exp(coord_to_dist(cx, c2), a)
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

# Final function to simulate a gaussian field
#' @export
sim_gauss_field_exp <- function(grid_nrow, a = 2, mu = -1) {

    # if mu == -1, then the default value was never changed, set mv = 0
    if (mu[1] == -1) { mean_vector <- rep(0, grid_nrow * grid_nrow) }
    else { mean_vector <- mu }

    Sigma <- get_cov_matrix_exp(grid_nrow, a)
    matrix(c(rmvnorm(1, mean_vector = mean_vector, sigma = Sigma)), byrow = TRUE, nrow = grid_nrow)

}

# Final function to simulate a gaussian field
#' @export
sim_gauss_field_gauss <- function(grid_nrow, a = 2, m = 1, mu = -1) {

    # if mu == -1, then the default value was never changed, set mv = 0
    if (mu[1] == -1) { mean_vector <- rep(0, grid_nrow * grid_nrow) }
    else { mean_vector <- mu }

    Sigma <- get_cov_matrix_gauss(grid_nrow, a, m)
    matrix(c(rmvnorm(1, mean_vector = mean_vector, sigma = Sigma)), byrow = TRUE, nrow = grid_nrow)

}

# Final function to simulate a gaussian field
#' @export
sim_gauss_field_tent <- function(grid_nrow, a = 3, sigma = 5, mu = -1) {

    # if mu == -1, then the default value was never changed, set mv = 0
    if (mu[1] == -1) { mean_vector <- rep(0, grid_nrow * grid_nrow) }
    else { mean_vector <- mu }

    Sigma <- get_cov_matrix_tent(grid_nrow, a, sigma)
    matrix(c(rmvnorm(1, mean_vector = mean_vector, sigma = Sigma)), byrow = TRUE, nrow = grid_nrow)

}

# Final function to simulate a gaussian field
#'@export
sim_gauss_field_sphere <- function(grid_nrow, a = 6, m = 1, mu = -1) {

    # if mu == -1, then the default value was never changed, set mv = 0
    if (mu[1] == -1) { mean_vector <- rep(0, grid_nrow * grid_nrow) }
    else { mean_vector <- mu }

    Sigma <- get_cov_matrix_sphere(grid_nrow, a, m)
    matrix(c(rmvnorm(1, mean_vector = mean_vector, sigma = Sigma)), byrow = TRUE, nrow = grid_nrow)

}