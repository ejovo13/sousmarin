# we want gaussian covariance
#
# C(h) = exp(-|h|^2/a^2)
#
# spectral density f(\omega) :
#
# f(w) = (a /(2 * sqrt(pi)))
#
#

library(pracma)

f_omega <- function(w, a = 1, d = 1) {
    ((a / (2 * sqrt(pi))) ** d) * exp(- (a ** 2 * abs(w) ** 2) / 4)
}

# Package sur GitHub
# devtools::install_github("EricMarcon/SpatDiv")
