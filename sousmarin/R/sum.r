library(Rcpp)

# Test implementations of c functions
sum_r <- function(x) {
    total <- 0
    for (i in seq_along(x)) {
        total <- total + x[i]
    }
    total
}

sum_c <- cppFunction('double sumC(NumericVector x) {
    int n = x.size();
    double total = 0;
    for (int i = 0; i < n; i++) {
        total += x[i];
    }
    return total;
    }')
