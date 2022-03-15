#include <Rcpp.h>
#include <iostream>
using namespace Rcpp;
// [[Rcpp::export]]
int double_me(int x) {
    return 2 * x;
}

// [[Rcpp::export]]
double mean_c(NumericVector vec) {
    double sum = 0;
    int n = vec.size();
    for (int i = 0; i < n; i++) {
        sum += vec[i];
    }

    return sum / n;
}

// [[Rcpp::export]]
void matrix_info(NumericMatrix mat) {

    std::cout << "Matrix has nrows: " << mat.nrow() << " and ncols: " << mat.ncol() << "\n";

}

NumericMatrix matmul(NumericMatrix A, NumericMatrix B) {

    // NumericMatrix m()


}