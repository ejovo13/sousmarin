library(pracma)

# f = function(x){

#   if (x>=0 && x<=1){
#      z= 6*x*(1-x)
#   } else {
#     z =0
#   }
#   z
# }

# #on simule pour une fonction sur un compact [0,1]
# #on majore par un rectangle

# acceptationrejet = function(f,N,a,b,M){
#   z = numeric(N)
#   j=0
#   while(j != N){
#     X = runif(1)*(b-a) + a
#     Y = runif(1)*M
#     if(Y<=f(X)){
#       z[j]=X
#       j=j+1
#     }
#   }
#   z
# }

# x=linspace(0,1)
# y=f(x)
# plot(x,y)
# acceptationrejet(f,10,0,1,1.5)


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

# Simulate n realizations of a normal with mean \mu and standard deviation \sd
#' @export
rnorm_acc <- function(n, mu = 0, sd = 1) {
    rnorm_acc_std(n) * sd + mu
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


