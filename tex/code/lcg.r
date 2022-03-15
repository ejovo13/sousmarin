

# Implement a simple LCG with m = 2^31 - 1, a = 12345678, b = 1


# Initialiser la graine (une variable globale) a 0
g_SEED_SOUSMARIN <- 0

# similaire a la fonction de R set.seed, mettre a jour
# la valeur de g_SEED_SOUSMARIN
set_seed <- function(seed) {
    assign("g_SEED_SOUSMARIN", seed, envir = .GlobalEnv)
}

# La fonction LCG pure qu'on a defini en parti 3
f_sousmarin <- function(x) {
    (12345678 * x + 1) %% (2^31 - 1)
}

# Generer une suite des variables de taille n en mettant a jour l'etat
# de la graine a chaque pas.
gen_suite <- function(n) {

    suite <- vector("numeric", n) # allouer un vector de taille n

    for (i in seq_len(n)) {
        x_i <- f_sousmarin(g_SEED_SOUSMARIN)
        suite[[i]] <- x_i
        set_seed(x_i)
    }

    suite
}

r_std_unif <- function(n) {

    suite <- vector("numeric", n)

    for (i in seq_len(n)) {
        x_i <- f_sousmarin(g_SEED_SOUSMARIN)
        suite[[i]] <- x_i / (2^31 - 1)
        set_seed(x_i)
    }

    suite
}

r_unif <- function(n, min = 0, max = 1) {

    spread <- max - min
    suite  <- vector("numeric", n)

    for (i in seq_len(n)) {
        x_i <- f_sousmarin(g_SEED_SOUSMARIN)
        suite[[i]] <- (x_i * spread) / (2^31 - 1) + min
        set_seed(x_i)
    }

    suite
}


#### Plotting functions
library(ggplot2)
library(tibble)
# library(tikz)

x <- runif(1E6)
df <- as_tibble(x)

library(tikzDevice)


# Write our graph to a .tex file

# tikz(file = "test.tex", width = 5, height = 3)

# df |> ggplot(aes(x)) +
#     geom_histogram(breaks = seq(0, 1, 0.0125), color = "black", fill = "#1076d4") +
#     # labs(title = "r\\_unif(n = 100000)") +
#     theme(plot.title = element_text(hjust = 0.5))

# dev.off()
