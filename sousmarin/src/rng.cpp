#include <Rcpp.h>
#include <cstdint>
// #include <iostream>
#include <thread>
#include <chrono>
#include <utility>
#include <climits>

using namespace Rcpp;

// store the results of random number generation
static double unif_res;


struct splitmix64_state {
	uint64_t s;
};

//asdfs [[Rcpp::export]]
typedef struct xoshiro256ss_state {
	uint64_t s[4];
} xor_rng;

// use more cryptographically secure functions to generate much better random numbers
uint64_t rol64(uint64_t x, int k)
{
	return (x << k) | (x >> (64 - k));
}

uint64_t splitmix64(struct splitmix64_state *state) {
	uint64_t result = (state->s += 0x9E3779B97f4A7C15);
	result = (result ^ (result >> 30)) * 0xBF58476D1CE4E5B9;
	result = (result ^ (result >> 27)) * 0x94D049BB133111EB;
	return result ^ (result >> 31);
}

/* This xoshiro256** algorithm was taken from the wikipedia page on xorshift generators
   The rest of the code interpreting these 64 random bits was all written by me, Evan Voyles */
uint64_t xoshiro256ss(struct xoshiro256ss_state *state)
{
	uint64_t *s = state->s;
	uint64_t const result = rol64(s[1] * 5, 7) * 9;
	uint64_t const t = s[1] << 17;

	s[2] ^= s[0];
	s[3] ^= s[1];
	s[1] ^= s[2];
	s[0] ^= s[3];

	s[2] ^= t;
	s[3] = rol64(s[3], 45);

	return result;
}


// I'd like to write a function that seeds all 256 bits of an xoshiro256ss when
// given a 32 bit seed (This is because R only support 32 bit integers)

// Here I need a top level xor_rng to be used in R

xor_rng R_XOR_RNG = {0x094518920928, 0x19823712585, 0x12098379385123, 0x1909827102835};

// Set the seed for a

//' @export
//[[Rcpp::export]]
void set_seed(uint32_t seed) {

    // First step is to "create" a 64 bit integer from a 32 bit input.

    uint64_t seed64;
    uint32_t *first32;
    uint32_t *last32;

    // point the pointers at the right address
    first32 = (uint32_t *) &seed64;
    last32 = first32 + 1;

    // assign the first 32 bits to seed
    *first32 = seed;
    *last32 = seed >> 16;

    // Now I supposedly have a 64 bit integer that I can use in the splitmix64 algorithm
    struct splitmix64_state sm = {seed64};

    uint64_t temp;

    for (int i = 0; i < 4; i++) {
        temp = splitmix64(&sm);
        R_XOR_RNG.s[i] = temp;
    }
}

//' @export
//[[Rcpp::export]]
void print_xor_state() {
    Rcout << "Printing the state of my rng\n";

    uint32_t *ptr32 = (uint32_t *) R_XOR_RNG.s;

    for (int i = 0; i < 8; i++) {
        Rcout << "RNG[" << i << "]: " << ptr32[i] << "\n";
    }
}

//
//bla [[Rcpp::export]]
// Ok this might actually be way harder than I want it to be, so for now
// I'm just going to use a different function xorunif...
double *user_unif_rand() {

    // using the R_XOR_RNG, let's generate a double!!
    uint64_t val = xoshiro256ss(&R_XOR_RNG);
    unif_res = (double) val / (double) ULONG_MAX;
    return &unif_res;
}

// Sample from X ~ U(0, 1)
double xorunif_std() {

    uint64_t val = xoshiro256ss(&R_XOR_RNG);
    return (double) val / (double) ULONG_MAX;
}

//' @export
// [[Rcpp::export]]
NumericVector xorunif(int n, double min = 0, double max = 1) {

    double spread = (max - min);
    NumericVector v (n);

    for (int i = 0; i < n; i++) {
        v[i] = xorunif_std() * spread + min;
    }

    return v;
}

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

