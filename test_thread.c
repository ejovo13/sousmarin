// #include <thread>
// #include <iostream>
#include "ejovo/ejovo_matrix.h"

// Try and use this file to learn how to
// parallelize a simple loop that sums the elements of a matrix


int main() {

    ejovo_seed();

    Matrix *m = Matrix_rand(1, 10);
    Matrix_print(m);


    double sum = 0;

    for (int i = 0; i < 10; i++) {
        printf("Matrix(0, %d): %lf\n", i, Matrix_at(m, 0, i));
        sum += Matrix_at(m, 0, i);
    }

    printf("Sum: %lf\n", sum);


    return 0;
}