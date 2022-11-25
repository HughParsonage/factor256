#include "factor256.h"

static SEXP rrorder(const unsigned char * xp, const unsigned char * yp, R_xlen_t N) {
  if (N >= INT_MAX) {
    SEXP ans = PROTECT(allocVector(REALSXP, N));
    double * restrict ansp = REAL(ans);
    R_xlen_t Table[256][256] = {0};
    for (R_xlen_t i = 0; i < N; ++i) {
      Table[xp[i]][yp[i]]++;
    }
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      R_xlen_t Table_xpi = Table[xp[i]][yp[i]];
      Table[xp[i]][yp[i]]--;
      ansp[Table_xpi] = i + 1;
    }
    UNPROTECT(1);
    return ans;
  }
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  unsigned int Table[256][256] = {0};
  for (int i = 0; i < N; ++i) {
    Table[xp[i]][yp[i]]++;
  }
  unsigned int t0 = 0;
  for (int j = 0; j < 256; ++j) {
    for (int k = 0; k < 256; ++k) {
      Table[j][k] += t0;
      t0 = Table[j][k];

    }
  }
  for (int i = N - 1; i >= 0; --i) {
    unsigned int Table_xpi = --Table[xp[i]][yp[i]];
    ansp[Table_xpi] = i + 1;
  }
  UNPROTECT(1);
  return ans;

}

static SEXP order1(SEXP x, SEXP y) {
  switch(TYPEOF(x)) {
  case RAWSXP:
    switch(TYPEOF(y)) {
    case RAWSXP:
      return rrorder(RAW(x), RAW(y), xlength(x));
    }
  }
  return R_NilValue;
}

// given an predecent order, find the new order
SEXP Corder2(SEXP o, SEXP x, SEXP y) {
  if (xlength(x) != xlength(y)) {
    error("xlength(x) != xlength(y)\t%lld, %lld", xlength(x), xlength(y));
  }
  switch(TYPEOF(o)) {
  case NILSXP:
    return order1(x, y);
  // case INTSXP:
  //   return iorder2(o, x, y);
  // case REALSXP:
  //   return dorder2(o, x, y);
  default:
    error("`o` was type '%s' but must be NULL or numeric.", type2char(TYPEOF(o)));
  }
  return R_NilValue;
}
