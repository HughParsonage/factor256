#ifndef factor256_H
#define factor256_H

#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <stdint.h> // for uint64_t rather than unsigned long long
#include <stdbool.h>
#include <math.h>
#include <ctype.h>

#define NA_INT -2147483648

void vminmax_i(int ans[2], const int * xp, R_xlen_t N);

#endif
