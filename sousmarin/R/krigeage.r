library(pracma)
library(purrr)

n <- 5 # start with 5 x 5 grid

# #' @export
# ind_to_coord <- function(index, n) {
#     c(floor((index - 1) / n) + 1, mod(index - 1, n) + 1)
# }


# This is an attempt to port these functions to cpp to increase the speed
# This is the slowest part of the entire gaussian simulation process.
#
library(Rcpp)
sourceCpp(file = "src/krigeage.cpp")

#' @export
cov_model_tent <- function(h, a, sigma) {
    sigma * sigma * (1 - abs(h) / a) * !(abs(h) > a)
}

#' @export
cov_model_tent_fac <- function(a, sigma) {
    function(h) {
        cov_model_tent(h, a, sigma)
    }
}

# Value at the origin m
# cancels at distance a
#' @export
cov_model_sphere <- function(h, a, m) {
    m * (1 - (1.5 * abs(h) / a - 0.5 * (abs(h) ** 3 / a ** 3))) * !(abs(h) > a)
}

cov_model_sphere_fac <- function(a, m) {
    function(h) {
        cov_model_sphere(h, a, m)
    }
}

cov_model_gauss_fac <- function(a, m) {
    function(h) {
        cov_model_gauss(h, a, m)
    }
}
