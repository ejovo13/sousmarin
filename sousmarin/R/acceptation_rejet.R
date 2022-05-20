library(pracma)

#on simule pour une fonction sur un compact [0,1]
#on majore par un rectangle

#' @export
acceptationrejet = function(f,N,a,b,M){
  z = numeric(N)
  j=0
  while(j != N){
    X = runif(1)*(b-a) + a
    Y = runif(1)*M
    if(Y<=f(X)){
      z[j]=X
      j=j+1
    }
  }
  z
}

# Generate a random variable following the quadratic dsitribution
# f(x) = 6x(1 - x) between [0, 1] via accept-reject using the bounded function
# g(x) = 1, with c = 1.5
#
# 1. generate a rv Y distributed as Unif(0, 1)
# 2. generate U ~ Unif(0, 1)
# 3. if U <= f(Y) / c g(Y), then accept our Y
#' @export
rquad <- function(n) {

    ind <- function(x) { 0 <= x & x <= 1 }
    # Uniform density between (0, 1)
    f <- function(x) {
        6 * x * (1 - x) * ind(x)
    }

    g <- function(x) {
        1 * ind(x)
    }

    c <- 1.5
    out <- rep(0, n)

    for (i in seq_len(n)) {

        reject <- TRUE
        z <- 0

        while (reject) {

            y <- runif(1)
            u <- runif(1)

            reject <- u > (f(y) / (c * g(y)))
            z <- y
        }

        out[[i]] <- z
    }

    out
}

# ====================== Normal Distributions ==================== #
# Simulate n realizations of a standard normal distribution
# using the acceptance-rejection method.
#' @export
rnorm_acc_std <- function(n) {

    x <- vector("numeric", n)

    for (i in 1:n) {

        reject <- TRUE
        z <- 0

        while (reject) {

            y1 <- rexp(1)
            y2 <- rexp(1)

            reject <- y2 < ((y1 - 1) * (y1 - 1) / 2)

            z <- abs(y1)
        }

        if (runif(1) < 0.5) { z <- -z }
        x[[i]] <- z
    }

    x

}

sourceCpp(code = "

    #include <Rcpp.h>

    using namespace Rcpp;

    //' @export
    // [[Rcpp::export]]
    NumericVector rnorm_acc_std_cpp(const int n) {

        NumericVector x (n);

        double z = 0;
        bool reject = false;
        double y1;
        double y2;

        for (int i = 0; i < n; i++) {

            z = 0;
            reject = true;

            while (reject) {

                y1 = R::rexp(1);
                y2 = R::rexp(1);

                reject = y2 < ((y1 - 1) * (y2 - 1) / 2);

                z = std::abs(y1);
            }

            z *= std::pow(-1, (R::runif(0, 1) < 0.5));
            x[i] = z;

        }

        return x;

    }

")

# Simulate n realizations of a normal with mean \mu and standard deviation \sd
#' @export
rnorm_acc <- function(n, mu = 0, sd = 1) {
    rnorm_acc_std_cpp(n) * sd + mu
}

#' @export
rnorm_acc_count <- function(n) {

    x <- vector("numeric", n)
    cnt_rej <- 0

    for (i in 1:n) {

        reject <- TRUE
        z <- 0

        while (reject) {

            y1 <- rexp(1)
            y2 <- rexp(1)

            reject <- y2 < ((y1 - 1) * (y1 - 1) / 2)

            z <- abs(y1)

            cnt_rej <- cnt_rej + 1
        }

        if (runif(1) < 0.5) { z <- -z }
        x[[i]] <- z
    }

    # In the count version of this function I want to return a list with
    # named elements $x and $cnt
    list("x" = x, "n_rej" = cnt_rej)

}


