# Run benchmarks of runif.

# This will eventually be used to test the difference between
# my implementation of a uniform random generator (xoroshiro**)
# and the mersenne twister!


library(pracma)
library(purrr)
library(microbenchmark)

# fn is a function used to generate a random variable n times, accepting
# only n as a parameter.
# So, we can use this to time any of our custom variables
time_rng <- function(fn = runif, exp_min = 2, exp_max = 6, n = 10) {

    N <- logspace(exp_min, exp_max, n)

    # mean_times <- vector("numeric", n)
    # for (i in seq_along(N)) {
    # }

    get_mean <- function(n) {
        bm <- microbenchmark({fn(n)})
        mean(bm$time)
    }

    mean_times <- map_dbl(N, get_mean)
    data.frame(n = N, time = mean_times)
}