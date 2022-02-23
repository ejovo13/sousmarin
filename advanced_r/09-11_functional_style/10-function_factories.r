# In this section we are going to learn about functions that return other functions!

# Can this be used for partial function application??

times <- function(x, k) {
    x * k
}

times10 <- partial(times, k = 10)
