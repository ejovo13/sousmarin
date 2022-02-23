## Functionals are functions like lapply that accept a vector and a function to
## return a vector

## 9.2 ============================
## purrr::map()

library(purrr)

triple <- \ (x) x * 3

map(1:3, triple) ## This will return a LIST
map_dbl(1:3, triple) ## returns a double VECTOR
# If you aren't going to use purrr, the author recommends using vapply as
# opposed to the other apply family of functions

# -- quick detour to learn about ...

helloWorld <- function(...) {
    arguments <- list(...)
    print(arguments)
    # paste(arguments)
}


## 9.3 ============================
## combining multiple functionals

## 9.4 ============================
## 18 variants of purrr::map()...

## 9.5 ============================
## New functional: purrr::reduce()

## 9.6 ============================
## Predicates

## 9.7 ============================
## Some other functionals!