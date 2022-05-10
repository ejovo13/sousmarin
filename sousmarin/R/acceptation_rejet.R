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
rnorm_acc <- function(n, mu = 0, sd = 1) {
    rnorm_acc_std(n) * sd + mu
}

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


# ==================== Plotting Normal Distributions ============== #
library(ggplot2)
library(tidyverse)
library(tibble)
library(purrr)
library(magrittr)
library(tikzDevice)

n <- 1e3
x_std <- rnorm_acc_std(n)
x_1_5 <- rnorm_acc(n, 1, 5)
x_10_12 <- rnorm_acc(n, 10, 12)


df <- tibble(x_std, x_1_5, x_10_12)
# df |>
#     ggplot(aes(x = x_std)) +
#     geom_density() +
#     geom_density(aes(x_1_5), col = "red") +
#     geom_density(aes(x_10_12), col = "blue") +
#     scale_color_manual("", values = "norm, norm2, norm3") +
#     labs()

m_list <- rnorm_acc_count(n)

count_rej <- function(n) {
    lst <- rnorm_acc_count(n)
    lst$n_rej
}

count_avg_rej <- function(n, n_exp = 10) {
    mean(map_dbl(rep(n, n_exp), count_rej))
}

#============= Linear Regression Analysis ================#
# Get the mx and b value of a linear regression given
# y and x data
lin_reg <- function(formula, data) {
  reg <- lm(formula, data = data)
  b <- reg$coefficients[1]
  m <- reg$coefficients[2]
  return(\(x) m * x + b)
}

rms <- function (y, y_hat) {
  sqrt(mse(y, y_hat))
}

# Calculate error MSE
mse <- function(y, y_hat) {
  err <- y - y_hat
  err <- err * err
  mean(err)
}

#================= Count the number of rejections ================#

# call count_rej
N <- 1:100
x_avg_counts <- map_dbl(N, count_avg_rej)
x_counts <- map_dbl(N, count_rej)

df_count <- tibble(N, nrej = x_counts)

df_count |>
    ggplot(aes(N, x_counts)) +
    geom_point()

lin_reg(nrej ~ N, df_count)

#==================== Try and draw a line for the model =========#
f <- lin_reg(nrej ~ N, data = df_count)

df_count %<>%
  mutate(nrej_vs_N = f(N)) %<>%
  mutate(link = as.factor(1:length(N)))


tikzDevice::tikz(file = "./avg_count_acc_rej.tex", width = 5, height = 3)

reg <- lm(nrej ~ N, data = df_count)

df_count |>
    ggplot(aes(N, nrej)) +
    geom_point(size = 0.25) +
    geom_line(aes(N, nrej_vs_N), size = 0.25) +
    geom_line(aes(N, nrej_vs_N), color = "red", alpha = 0.3) +
    geom_segment(aes(N, nrej_vs_N, xend = N, yend = nrej), color = "red") +
    labs(
        title = "Moyenne des rejections",
        #   subtitle = paste("$\\texttt{nrej}(n) \\approx", regression$N, "n + 1.3x"),
        #   subtitle = "hi",
        subtitle = "Pour n realisations d'une variable aleatoire normale",
        x = "$n$",
        y = paste("$\\texttt{nrej}(n) \\approx ", round(reg$coefficients["N"], digits = 2), "n$", sep = ""),
        #   y = "$f(x) = 1.16x - 549.56$"
        )

dev.off()