#include <omp.h>
#include <iostream>

#define MAX 10

int parallel_loop(int);

int main() {
    // int count = 0;
    // #pragma omp parallel num_threads(MAX)
    // {
    //     #pragma omp atomic
    //     count++;
    // }
    // std::cout << "Number of threads: " << count << std::endl;

    // #pragma omp barrier

    std::cout << "Total sum: " << parallel_loop(8) << "\n";
}


int parallel_loop(int nthreads = 10) {

    int i = 0;
    int thread_sum = 0;
    int maxn = 1e6;
    long long total_sum = 0;

    omp_set_num_threads(nthreads);

    #pragma omp parallel private(i, thread_sum) shared(maxn, total_sum)
    {

        // #pragma omp master
        // std::cout << "nthreads: " << omp_get_num_threads();

        #pragma omp for
        for (i = 0; i <= maxn; i++) {
            thread_sum += i;
        }

        // #pragma omp barrier

        #pragma omp atomic
        total_sum += thread_sum;
        // #pragma omp critical
        // {
        // }

    }

    return total_sum;


}