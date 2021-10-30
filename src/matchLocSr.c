#include "factor256.h"



int equal_width(SEXP x, SEXP y) {
  const SEXP * xp = STRING_PTR(x);
  const SEXP * yp = STRING_PTR(y);
  if (xlength(x) >= INT_MAX || xlength(y) >= INT_MAX) {
    return INT_MAX;
  }
  int N = length(x);
  int M = length(y);
  if (M == 0 || N == 0) {
    return 1;
  }
  const int w = length(xp[0]);
  for (int i = 1; i < N; ++i) {
    int ni = length(xp[i]);
    if (__builtin_expect(ni - w, 0)) {
      return i + 1;
    }
  }
  for (int i = 1; i < M; ++i) {
    int ni = length(yp[i]);
    if (__builtin_expect(ni - w, 0)) {
      return -i - 1;
    }
  }
  return 0;
}

SEXP CSortFw16(SEXP x, SEXP dd) {
  R_xlen_t N = xlength(x);
  if (N <= 1) {
    return x;
  }
  // counting sort on d'th digit
  const int d = asInteger(dd);
  const SEXP * xp = STRING_PTR(x);
  const int w = length(xp[0]);
  R_xlen_t Table[256] = {0};
  for (R_xlen_t i = 0; i < N; ++i) {
    const char * xpi = CHAR(xp[i]);
    unsigned int xpid = xpi[d];
    Table[xpid] += 1;
  }
  for (int j = 1; j < 256; ++j) {
    Table[j] += Table[j - 1];
  }
  SEXP ans = PROTECT(allocVector(STRSXP, N));
  SEXP * restrict ansp = STRING_PTR(ans);
  for (R_xlen_t i = N - 1; i >= 0; --i) {
    const char * xpi = CHAR(xp[i]);
    unsigned int xpid = xpi[d];
    R_xlen_t Table_xpi = --Table[xpid];
    ansp[Table_xpi] = xp[i];
  }
  UNPROTECT(1);
  return ans;
}

R_xlen_t isSorted_d(SEXP x, SEXP dd) {
  R_xlen_t N = xlength(x);
  if (N <= 1) {
    return 0;
  }
  const SEXP * xp = STRING_PTR(x);
  // counting sort on d'th digit
  const int d = asInteger(dd);
  const int w = length(xp[0]);
  if (d >= w) {
    return 1;
  }

  for (R_xlen_t i = 1; i < N; ++i) {
    if (length(xp[i]) != w) {
      ++i;
      continue;
    }
    const char * xi0 = CHAR(xp[i - 1]);
    const char * xi1 = CHAR(xp[i]);

    unsigned char xi0j = xi0[0];
    unsigned char xi1j = xi1[0];
    int sign_diff = 0;
    for (int j = 0; j < d; ++j) {
      xi0j = xi0[j];
      xi1j = xi1[j];
      sign_diff = xi1j - xi0j;
      if (sign_diff) {
        break;
      }
    }
    if (sign_diff) {
      if (sign_diff < 0) {
        return i + 1;
      }
      // sorted by virtue of earlier digits
      continue;
    }
    char x0 = xi0[d];
    char x1 = xi1[d];
    if (x0 == x1) {
      return i + 1;
    }
  }
  return 0;
}

SEXP CisSorted_d(SEXP x, SEXP dd) {
  return ScalarLength(isSorted_d(x, dd));
}

SEXP matchLocSr(SEXP x, SEXP y, SEXP Check) {
  /*
  // given x and y character vectors of equal width
  const int checks = asInteger(Check);
  if (checks) {
    int ew = equal_width(x, y);
    if (ew) {
      error("matchLocSr assertion failed(unequal_width): Error number %d", ew);
    }
  }
  const SEXP * xp = STRING_PTR(x);
  const SEXP * yp = STRING_PTR(y);
   */
  return ScalarInteger(1);
}

