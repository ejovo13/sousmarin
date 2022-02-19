#include <R_ext/Random.h>
#include <stdint.h>

// typedef struct xoshiro256ss_state {
// 	uint64_t s[4];
// } xor_rng;

// I need to transform the seed from a Int32 to something that has
// 256 bits of state.
// I could set the one of the integers to this value and I guess hope that
// it will propogate?

static double x;

double *user_unif_rand() {
    x = 0.5;
    return &x;
}

// // not my code
// static Int32 seed;
// static double res;
// static int nseed = 1;

// double * user_unif_rand()
// {
//     seed = 69069 * seed + 1;
//     res = seed * 2.32830643653869e-10;
//     return &res;
// }