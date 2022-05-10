# Use the box muller transform to create a single normally distributed variable

e_rnorm <- function(u1, u2) {
    sqrt(-2 * log(u1)) * cos(2 * pi * u2)
}

library(purrr)

