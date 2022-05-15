# Implement a simple LCG with m = 2^31 - 1, a = 12345678, b = 1


# Initialiser la graine (une variable globale) a 0
assign("g_SEED_SOUSMARIN", 0, envir = .GlobalEnv)

# similaire a la fonction de R set.seed, mettre a jour
# la valeur de g_SEED_SOUSMARIN
#' @export
set_seed <- function(seed) {
    assign("g_SEED_SOUSMARIN", seed, envir = .GlobalEnv)
}

# Afficher a l'ecran la valeur de g_SEED_SOUSMARIN
#' @export
get_seed <- function() {
    g_SEED_SOUSMARIN
}

# La fonction LCG pure qu'on a defini en parti 3
#' @export
f_sousmarin <- function(x) {
    (12345678 * x + 1) %% (2^31 - 1)
}

# Generer une suite des variables de taille n en mettant a jour l'etat
# de la graine a chaque pas.
#' @export
gen_suite <- function(n) {

    suite <- vector("numeric", n) # allouer un vector de taille n

    for (i in seq_len(n)) {
        x_i <- f_sousmarin(g_SEED_SOUSMARIN)
        suite[[i]] <- x_i
        set_seed(x_i)
    }

    suite
}

#' @export
r_std_unif <- function(n) {

    suite <- vector("numeric", n)

    for (i in seq_len(n)) {
        x_i <- f_sousmarin(g_SEED_SOUSMARIN)
        suite[[i]] <- x_i / (2^31 - 1)
        set_seed(x_i)
    }

    suite
}

#' @export
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

runif_lcg_75 = function(xo,n){
  x =vector("numeric", n)
  x[1] = xo
  for (i in seq_len(n)){
    x[i+1] = (75*x[i] + 74) %% (2**16 + 1)
  }
  x/(2**16 + 1)
}