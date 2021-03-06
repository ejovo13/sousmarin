# ==================== Plotting Normal Distributions ============== #
library(sousmarin)
library(ggplot2)
library(tidyverse)
library(tibble)
library(purrr)
library(magrittr)
library(tikzDevice)
library(pracma)


# ====================== Acceptation rejet for f(x) = 6x(1 - x) =====#
f = function(x){

  if (x>=0 && x<=1){
     z= 6*x*(1-x)
  } else {
    z =0
  }
  z
}

g <- function(x) {
    1.5
}

x=linspace(0,1)
y=f(x)
plot(x, y, type = "l")
lines(x, map_dbl(x, g), col = "red")
z <- acceptationrejet(f,1000,0,1,1.5)
hist(x)

# ================ The previous function clearly doesnt work =============#
# Here is an actual proper density using ggplot2 and rquad to simulate random
# variables from the quadratic density function f(x) = 6x(1 - x)
f_quad <- function(x) {
    6 * x * (1 - x) * (x >= 0 & x <= 1)
}

# tikzDevice::tikz(file = "./rquad_sim.tex", width = 5, height = 3)

x <- rquad(1e5)
tibble(x) |> ggplot(aes(x)) +
    geom_histogram(aes(y = stat(density)), fill = "#5087C1") +
    geom_function(fun = f_quad, size = 1.) +
    labs(y = "fréquence", title = "Simulation d'une loi quadratique", subtitle = "Densité f(x) = 6x(1 - x), n = 10,000")

ggsave("rquad_sim.png")

# dev.off()

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


# tikzDevice::tikz(file = "./avg_count_acc_rej.tex", width = 5, height = 3)

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

# dev.off()