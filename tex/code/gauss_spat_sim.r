# Let's simulate some gaussian spatial grids
library(sousmarin)
library(pracma)
library(testit)

n <- 9 # 60 takes about a MINUTE maybe more.
        # I wonder if I should write a function that takes the CHOL
        # factorization to avoid that computational step??
        # Then we're really just left with matrix multiplication


mu <- rep(0, n * n)
# sigma <- get_cov_matrix_gauss(n, 2.7, 1)
sigma <- get_cov_matrix_exp(n, 3)
# sigma <- get_cov_matrix_sphere(n, 6)
# sigma <- get_cov_matrix_tent(n, 3, 5)
L <- chol(sigma)

Z <- matrix(c(rmvnorm_chol(1, L, mu)), nrow = n, byrow = TRUE)
image(Z, col = hcl.colors(5, palette = "plasma"), bty = "n", xaxt = "n", yaxt = "n")

# # let's create a 6 x 6 image of 60x60 sections
grid_n <- 2

big_image <- matrix(rep(0, grid_n * grid_n * n * n), nrow = n * grid_n)
for(i in 1:grid_n) {
    for (j in 1:grid_n) {
        big_image[((i - 1) * n + 1):(i * n), ((j - 1) * n + 1):(j * n)] = matrix(c(rmvnorm_chol(1, L, mu)), nrow = n, byrow = TRUE)
    }
}

image(big_image, col = hcl.colors(5, palette = "viridis"), bty = "n", xaxt = "n", yaxt = "n")