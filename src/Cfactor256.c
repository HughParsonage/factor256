#include "factor256.h"

int cmpfunc(const void * ap, const void * bp) {
  int a = *(int*)ap;
  int b = *(int*)bp;
  // return a - b; // may overflow?
  return (a < b) ? -1 : (a == b ? 0 : 1);
}

int binary_find(int key, int * xp, int N) {
  int * res = (int *)bsearch(&key, xp, N, sizeof(int), cmpfunc);
  if (res) {
    return res - &xp[0];
  }
  return -1;
}

bool isntRaw(SEXP x) {
  return TYPEOF(x) != RAWSXP;
}

int subtract(int a, int b) {
  int64_t aa = a, bb = b;
  return aa - bb;
}






SEXP BSearch(SEXP aa, SEXP xx) {
  //
  int * xp = INTEGER(xx);
  int * ap = INTEGER(aa);
  R_xlen_t N = xlength(aa);
  int n = length(xx);
  if (n >= 256) {
    return R_NilValue;
  }
  SEXP ans = PROTECT(allocVector(RAWSXP, N));
  unsigned char * restrict ansp = RAW(ans);
  int g[256] = {0};
  for (int j = 0; j < n; ++j) {
    g[j] = xp[j];
  }
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = 0;
    int api = ap[i];
    for (int j = 1; j < 256; ++j) {
      if (api == g[j]) {
        ansp[i] = j;
        break;
      }
    }
    // ansp[i] = binary_find(ap[i], xp, n);
  }
  UNPROTECT(1);
  return ans;
}

#define FACTOR256_STACK_SIZE 32768
#define FACTOR256_STACK_SIZD 32767

SEXP CStackMatch(SEXP x, SEXP ux) {
  // match(x, ux, nomatch = 0L) but type raw
  if (!isInteger(x) || !isInteger(ux)) {
    error("Not int.");
  }
  const R_xlen_t N = xlength(x);
  const int M = length(ux);
  if (M >= 256) {
    error("M >= 256");
  }
  const int * xp = INTEGER(x);
  const int * uxp = INTEGER(ux);
  int xminmax[2] = {0};
  vminmax_i(xminmax, uxp, M);
  const int xmin = xminmax[0];
  const int xmax = xminmax[1];
  const unsigned int rang = subtract(xmax, xmin);

  if (rang >= FACTOR256_STACK_SIZE) {
    return R_NilValue;
  }
  unsigned char tbl[FACTOR256_STACK_SIZE] = {0};
  // if (tbl == NULL) {
  //   free(tbl);
  //   return R_NilValue;
  // }
  for (int j = 0; j < M; ++j) {
    int upi = uxp[j] - xmin;
    tbl[upi] = (j + 1) & 255;
  }
  SEXP ans = PROTECT(allocVector(RAWSXP, N));
  unsigned char * restrict ansp = RAW(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    unsigned int xpi = xp[i] - xmin;
    ansp[i] = tbl[xpi & FACTOR256_STACK_SIZD];
  }
  UNPROTECT(1);
  return ans;
}


unsigned char lgl2raw(int x) {
  return x == NA_INTEGER ? 2 : x;
}

int raw2lgl(unsigned char x) {
  return x == 2 ? NA_INTEGER : x;
}

SEXP Clogical2factor256(SEXP x) {
  R_xlen_t N = xlength(x);
  const int * xp = LOGICAL(x);
  SEXP ans = PROTECT(allocVector(RAWSXP, N));
  unsigned char * ansp = RAW(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = lgl2raw(xp[i]);
  }
  UNPROTECT(1);
  return ans;
}

SEXP Cfactor2562logical(SEXP x) {
  R_xlen_t N = xlength(x);
  const unsigned char * xp = RAW(x);
  SEXP ans = PROTECT(allocVector(LGLSXP, N));
  int * ansp = LOGICAL(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = raw2lgl(xp[i]);
  }
  UNPROTECT(1);
  return ans;
}

SEXP Cint2factor256(SEXP x) {
  R_xlen_t N = xlength(x);
  const int * xp = INTEGER(x);
  SEXP ans = PROTECT(allocVector(RAWSXP, N));
  unsigned char * ansp = RAW(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    unsigned int xpi = xp[i];
    ansp[i] = xpi;
  }
  UNPROTECT(1);
  return ans;
}

SEXP Cfactor256_in(SEXP x, SEXP tbl, SEXP Not) {
  const bool no = asLogical(Not);
  if (isntRaw(x) || isntRaw(tbl)) {
    error("(Internal error): x or tbl not RAWSXP"); // # nocov
  }
  if (xlength(tbl) >= 256) {
    error("xlength(tbl) >= 256 (duplicated entries to tbl)"); // # nocov
  }
  R_xlen_t N = xlength(x);
  int m = length(tbl);
  const unsigned char * xp = RAW(x);
  const unsigned char * yp = RAW(tbl);

  bool w[256] = {0};
  for (int j = 0; j < 256; ++j) {
    w[j] = no;
  }
  for (int j = 0; j < m; ++j) {
    w[yp[j]] = no ? 0 : 1;
  }
  SEXP ans = PROTECT(allocVector(LGLSXP, N));
  int * restrict ansp = LOGICAL(ans);

  for (R_xlen_t i = 0; i < N; ++i) {
    unsigned int xpi = xp[i];
    ansp[i] = w[xpi];
  }
  UNPROTECT(1);
  return ans;
}

SEXP C_asInteger0(SEXP x) {
  if (isntRaw(x)) {
    error("Internal error(C_asInteger0): x was of type '%s' but must be type RAWSXP.", type2char(TYPEOF(x))); // # nocov
  }
  R_xlen_t N = xlength(x);
  const unsigned char * xp = RAW(x);
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    unsigned int xpi = xp[i];
    ansp[i] = xpi == 0 ? NA_INTEGER : xpi;
  }
  UNPROTECT(1);
  return ans;
}



