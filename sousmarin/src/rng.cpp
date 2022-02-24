#include <Rcpp.h>
#include <cstdint>
// #include <iostream>
#include <thread>
#include <chrono>
#include <utility>

using namespace Rcpp;

//asdfs [[Rcpp::export]]
typedef struct xoshiro256ss_state {
	uint64_t s[4];
} xor_rng;

//' Multiply a number by two
//'
//'
//' @export
//[[Rcpp::export]]
int double_me(int x) {
    return 2 * x;
}

//' @export
//[[Rcpp::export]]
void say_hello(int x) {
    Rcout << "You entered: " << x << std::endl;
}

void f1(int n)
{
    for (int i = 0; i < 5; ++i) {
        std::cout << "Thread 1 executing\n";
        ++n;
        std::this_thread::sleep_for(std::chrono::milliseconds(10));
    }
}

void f2(int& n)
{
    for (int i = 0; i < 5; ++i) {
        std::cout << "Thread 2 executing\n";
        ++n;
        std::this_thread::sleep_for(std::chrono::milliseconds(10));
    }
}


// Alright let's try and get multiple threads going just to learn how to work with this
//' @export
//[[Rcpp::export]]
void test_threads() {

    int n = 0;
    std::thread t1(f1, n + 1);
    t1.join();

}

void test_omp() {

    int count = 0;




}

