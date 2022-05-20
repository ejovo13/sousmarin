#include <Rcpp.h>
#include <cmath>
#include <iterator>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector ind_to_coord(const int index, const int grid_nrow) {

    NumericVector v = {std::floor( (index - 1) / grid_nrow + 1), (index - 1) % grid_nrow + 1};
    return v;

}

// [[Rcpp::export]]
double coord_to_dist(const NumericVector& p1, const NumericVector& p2) {
    return std::sqrt(std::pow(p1[0] - p2[0], 2) + std::pow(p1[1] - p2[1], 2));
}

/* is this a fucking joke */
// [[Rcpp::export]]
double cov_model_exp_cpp(const double d, double a = 2) {
    return std::exp(-(d / a));
}

//' @export
// [[Rcpp::export]]
double cov_model_gauss(const double d, double a, double m) {
    return m * std::exp(-(std::pow(d, 2) / std::pow(a, 2)));
}

//' @export
// [[Rcpp::export]]
double cov_model_sphere(const double d, double a, double m) {
    return m * (1 - (1.5 * d / a - (0.5 * (std::pow(d, 3) / std::pow(a, 3))))) * !(d > a);
}

//' @export
// [[Rcpp::export]]
double cov_model_tent(const double d, double a, double sigma) {
    return std::pow(sigma, 2) * (1 - d / a) * !(d > a);
}

/* testing */
//' @export
// [[Rcpp::export]]
NumericVector get_cov_row_exp(const int i, const int grid_nrow, const double a = 2) {

    const int len_out = std::pow(grid_nrow, 2) - i + 1;
    NumericVector x (len_out);

    auto c1 = ind_to_coord(i, grid_nrow);

    for (int j = 0; j < len_out; j++) {
        x[j] = cov_model_exp_cpp(coord_to_dist(c1, ind_to_coord(j + i, grid_nrow)), a);
    }

    return x;
}

//' @export
// [[Rcpp::export]]
NumericVector get_cov_row_gauss(const int i, const int grid_nrow, const double a = 2, const double m = 1) {

    const int len_out = std::pow(grid_nrow, 2) - i + 1;
    NumericVector x (len_out);

    auto c1 = ind_to_coord(i, grid_nrow);

    for (int j = 0; j < len_out; j++) {
        x[j] = cov_model_gauss(coord_to_dist(c1, ind_to_coord(j + i, grid_nrow)), a, m);
    }

    return x;
}

//' @export
// [[Rcpp::export]]
NumericVector get_cov_row_tent(const int i, const int grid_nrow, const double a = 2, const double sigma = 1) {

    const int len_out = std::pow(grid_nrow, 2) - i + 1;
    NumericVector x (len_out);

    auto c1 = ind_to_coord(i, grid_nrow);

    for (int j = 0; j < len_out; j++) {
        x[j] = cov_model_tent(coord_to_dist(c1, ind_to_coord(j + i, grid_nrow)), a, sigma);
    }

    return x;
}

//' @export
// [[Rcpp::export]]
NumericVector get_cov_row_sphere(const int i, const int grid_nrow, const double a = 2, const double m = 1) {

    const int len_out = std::pow(grid_nrow, 2) - i + 1;
    NumericVector x (len_out);

    auto c1 = ind_to_coord(i, grid_nrow);

    for (int j = 0; j < len_out; j++) {
        x[j] = cov_model_sphere(coord_to_dist(c1, ind_to_coord(j + i, grid_nrow)), a, m);
    }

    return x;
}

// Use the exponential function as the covariance
//' @export
// [[Rcpp::export]]
NumericMatrix get_cov_matrix_exp(const int grid_nrow, const double a = 2) {

    const int n_sq = std::pow(grid_nrow, 2);
    NumericMatrix Sigma ( n_sq, n_sq );

    for (int i = 1; i <= n_sq; i++) {

        NumericVector row = get_cov_row_exp(i, grid_nrow, a);

        for (int j = i; j <= n_sq; j++) {
            Sigma(i - 1, j - 1) = row(j - i);
        }
    }

    auto SigmaT = transpose(Sigma);

    for (int i = 0; i < n_sq; i++) {
        for (int j = 0; j < n_sq; j++) {
            Sigma(i, j) += SigmaT(i, j);
        }
    }

    // subtract one from the diagonals
    for (int d = 0; d < n_sq; d++) {
        Sigma(d, d) -= 1;
    }

    return Sigma;
}

// Use the gauss function as the covariance
//' @export
// [[Rcpp::export]]
NumericMatrix get_cov_matrix_gauss(const int grid_nrow, const double a = 2, const double m = 1) {

    const int n_sq = std::pow(grid_nrow, 2);
    NumericMatrix Sigma ( n_sq, n_sq );

    for (int i = 1; i <= n_sq; i++) {

        NumericVector row = get_cov_row_gauss(i, grid_nrow, a, m);

        for (int j = i; j <= n_sq; j++) {
            Sigma(i - 1, j - 1) = row(j - i);
        }
    }

    auto SigmaT = transpose(Sigma);

    for (int i = 0; i < n_sq; i++) {
        for (int j = 0; j < n_sq; j++) {
            Sigma(i, j) += SigmaT(i, j);
        }
    }

    // subtract one from the diagonals
    for (int d = 0; d < n_sq; d++) {
        Sigma(d, d) -= 1;
    }

    return Sigma;
}

// Use the gauss function as the covariance
//' @export
// [[Rcpp::export]]
NumericMatrix get_cov_matrix_sphere(const int grid_nrow, const double a = 2, const double m = 1) {

    const int n_sq = std::pow(grid_nrow, 2);
    NumericMatrix Sigma ( n_sq, n_sq );

    for (int i = 1; i <= n_sq; i++) {

        NumericVector row = get_cov_row_sphere(i, grid_nrow, a, m);

        for (int j = i; j <= n_sq; j++) {
            Sigma(i - 1, j - 1) = row(j - i);
        }
    }

    auto SigmaT = transpose(Sigma);

    for (int i = 0; i < n_sq; i++) {
        for (int j = 0; j < n_sq; j++) {
            Sigma(i, j) += SigmaT(i, j);
        }
    }

    // subtract one from the diagonals
    for (int d = 0; d < n_sq; d++) {
        Sigma(d, d) -= 1;
    }

    return Sigma;
}


// Use the gauss function as the covariance
//' @export
// [[Rcpp::export]]
NumericMatrix get_cov_matrix_tent(const int grid_nrow, const double a = 2, const double sigma = 1) {

    const int n_sq = std::pow(grid_nrow, 2);
    NumericMatrix Sigma ( n_sq, n_sq );

    for (int i = 1; i <= n_sq; i++) {

        NumericVector row = get_cov_row_tent(i, grid_nrow, a, sigma);

        for (int j = i; j <= n_sq; j++) {
            Sigma(i - 1, j - 1) = row(j - i);
        }
    }

    auto SigmaT = transpose(Sigma);

    for (int i = 0; i < n_sq; i++) {
        for (int j = 0; j < n_sq; j++) {
            Sigma(i, j) += SigmaT(i, j);
        }
    }

    // subtract one from the diagonals
    for (int d = 0; d < n_sq; d++) {
        Sigma(d, d) -= 1;
    }

    return Sigma;
}