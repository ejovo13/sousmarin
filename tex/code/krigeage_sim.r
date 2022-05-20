# Let's go ahead and simulate some krigeage in a 10 x 10 cell
#
#
#
#
#
#
library(sousmarin)
library(purrr)

n <- 10
a <- 2
n_obs <- 5

Z <- sim_gauss_field_exp(10, 2)

# Now let's sample n_obs from Z
obs_ind <- force(sample(1:(n * n), n_obs))
list_coords <- map(obs_ind, function(ind) { ind_to_coord(ind, n) })

round(get_lambda(list_coords, obs_ind[1], n, a), 2)


