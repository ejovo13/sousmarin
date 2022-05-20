# Let's go ahead and simulate some krigeage in a 10 x 10 cell
#
#
#
#
#
#
library(sousmarin)
library(purrr)
library(testit)

n <- 50
a <- 2
n_obs <- 100

# Z <- sim_gauss_field_exp(n, a, mu = rep(10, n * n))
Z <- sim_gauss_field_exp(n, a)


# Now let's sample n_obs from Z
obs_ind <- force(sample(1:(n * n), n_obs))
list_coords <- map(obs_ind, function(ind) { ind_to_coord(ind, n) })
obs_values <- Z[obs_ind]


image(Z, xaxt = "n", yaxt = "n", bty = "n")

Z[-obs_ind] = 100
image(Z, xaxt = "n", yaxt = "n", bty = "n")

# Z[-obs_ind] = 0

lhs <- get_cov_matrix_lambda(list_coords, a)

# Now I want to construct the right hand matrix
# matrix is n_obs x (n^2)
rhs <- matrix(rep(0, n_obs * n * n), nrow = n_obs)

for (x in 1:(n * n)) {
    rhs[, x] <- get_cov_obs(list_coords, x, n, a)
}

# Lambda matrix is now just solving the system of equations
Lambda <- solve(lhs, rhs)

obs_ind
obs_values

Z_hat <- matrix(c(obs_values %*% Lambda), byrow = TRUE, nrow = n)
image(Z_hat, xaxt = "n", yaxt = "n", bty = "n")
