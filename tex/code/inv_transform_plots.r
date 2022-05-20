library(sousmarin)
library(purrr)
library(tidyverse)

# Here we use the function r_exp to create some exponential distributions and
# test to make sure that the moyenne empirique is 1/lambda

Lambda <- 1:30
n <- 1000

# Now let's store the trajectories in a list, compute their mean, and return a vector
mean_traj <- unlist(map(map(Lambda, function(lambda) { rexp_inv(n, lambda) }), mean))
mean_theo <- 1 / Lambda

df <- tibble(lambda = Lambda, mu_theo = mean_theo, mu_traj = mean_traj)

df |> ggplot(aes(lambda, mu_theo)) +
    geom_line() +
    geom_point(aes(lambda, mu_traj), col = "red") +
    labs(y = "espérance", title = "Moyennes empiriques des lois exponentielles") +
    theme(panel.background = element_blank())

ggsave(filename = "moyenne_empiriques_exp.png", device = NULL, width = 5, height = 3)