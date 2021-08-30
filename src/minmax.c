#include "factor256.h"

void vminmax_i(int ans[2], const int * xp, R_xlen_t N) {
  int xp0 = xp[0];
  int min = xp0;
  int max = xp0;
  for (R_xlen_t i = 1; i < N; ++i) {
    int xpi = xp[i];
    min = xpi < min ? xpi : min;
    max = xpi > max ? xpi : max;
  }
  ans[0] = min;
  ans[1] = max;
}

