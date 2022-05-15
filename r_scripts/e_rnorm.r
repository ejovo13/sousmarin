# Use the box muller transform to create a single normally distributed variable

rnorm_box_muller <- function(u1, u2) {
    sqrt(-2 * log(u1)) * cos(2 * pi * u2)
}
