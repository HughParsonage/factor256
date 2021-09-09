#include "factor256.h"

#define FACTOR256_STACK_SIZE 32768
#define FACTOR256_STACK_SIZD 32767

bool isntRaw(SEXP x) {
  return TYPEOF(x) != RAWSXP;
}

int subtract(int a, int b) {
  int64_t aa = a, bb = b;
  return aa - bb;
}

SEXP ScalarLength(R_xlen_t i) {
  return (i <= INT_MAX) ? ScalarInteger(i) : ScalarReal(i);
}

R_xlen_t isntSorted256(SEXP x, bool s) {
  R_xlen_t N = xlength(x);
  if (isntRaw(x) || N <= 1) {
    return 0;
  }
  const unsigned char * xp = RAW(x);
  for (R_xlen_t i = 1; i < N; ++i) {
    unsigned int xpi0 = xp[i - 1];
    unsigned int xpi1 = xp[i];
    bool unsorted = s ? xpi0 >= xpi1 : xpi0 > xpi1;
    if (unsorted) {
      return i + 1;
    }
  }
  return 0;
}

SEXP CisntSorted256(SEXP x, SEXP strictly) {
  const bool s = asLogical(strictly);
  return ScalarLength(isntSorted256(x, s));
}

R_xlen_t nonDuplicated(SEXP x, SEXP h) {
  // ensure for every hash
  const SEXP * xp = STRING_PTR(x);
  const int * hp = INTEGER(h);

  R_xlen_t N = xlength(x);
  if (xlength(h) != N) {
    return 1;
  }
  SEXP ux = PROTECT(allocVector(STRSXP, 256));
  SEXP * uxp = STRING_PTR(ux);
  for (int j = 0; j < 256; ++j) {
    uxp[j] = NA_STRING;
  }
  R_xlen_t o = 0;
  for (R_xlen_t i = 0; i < N; ++i) {
    int h256 = hp[i] & 255;
    if (uxp[h256] != NA_STRING && uxp[h256] != xp[i]) {
      o = i + 1;
      break;
    }
    uxp[h256] = xp[i];
  }
  UNPROTECT(1);
  return o;
}

SEXP C_nonDuplicated(SEXP x, SEXP h) {
  R_xlen_t o = nonDuplicated(x, h);
  return o <= INT_MAX ? ScalarInteger(o) : ScalarReal(o);
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
  // Not
  //   0: in
  //   1: notin
  //   2: ein
  //   3: enotin
  const unsigned int inot = asInteger(Not);
  const bool no = inot & 1;
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

void do_tab256(R_xlen_t * tbl, const unsigned char * xp, R_xlen_t N) {
  if (tbl == NULL) {
    return;
  }
  for (int j = 0; j < 256; ++j) {
    tbl[j] = 0;
  }
  for (R_xlen_t i = 0; i < N; ++i) {
    int xpi = xp[i];
    tbl[xpi] += 1;
  }
}

void do_tab256u(unsigned int * tbl, const unsigned char * xp, R_xlen_t N) {
  if (tbl == NULL) {
    return;
  }
  for (int j = 0; j < 256; ++j) {
    tbl[j] = 0;
  }
  for (R_xlen_t i = 0; i < N; ++i) {
    int xpi = xp[i];
    tbl[xpi] += 1;
  }
}

SEXP Ctabulate256(SEXP x) {
  if (isntRaw(x)) {
    return R_NilValue;
  }
  R_xlen_t N = xlength(x);
  const unsigned char * xp = RAW(x);
  R_xlen_t Table[256] = {0};
  do_tab256(Table, xp, N);
  bool needs_double = false;
  for (int i = 0; i < 256; ++i) {
    if (Table[i] > INT_MAX) {
      needs_double = true;
    }
  }
  SEXP ans = PROTECT(allocVector(needs_double ? REALSXP : INTSXP, 256));
  if (needs_double) {
    double * restrict ansp = REAL(ans);
    for (int i = 0; i < 256; ++i) {
      ansp[i] = Table[i];
    }
  } else {
    int * restrict ansp = INTEGER(ans);
    for (int i = 0; i < 256; ++i) {
      ansp[i] = Table[i];
    }
  }
  UNPROTECT(1);
  return ans;
}

unsigned int nxt_2pwr(unsigned int v) {
  v--;
  v |= v >> 1;
  v |= v >> 2;
  v |= v >> 4;
  v |= v >> 8;
  v |= v >> 16;
  // don't ++v because we want to use &
  return v;
}


SEXP Ctabulate256_levels(SEXP x, SEXP Nmax, SEXP dotInterval) {
  // Returns a vector of length 256 equivalent to table(x) but additionally
  // stops when the table reaches Nmax counts, which are checked every
  //
  if (isntRaw(x)) {
    return R_NilValue;
  }
  const int nmax = asInteger(Nmax);
  const unsigned int interval = nxt_2pwr(asInteger(dotInterval));
  const unsigned char * xp = RAW(x);
  if (xlength(x) >= UINT_MAX) {
    return R_NilValue; // # nocov
  }
  unsigned int N = xlength(x);
  unsigned int Table[256] = {0};
  for (unsigned int i = 0; i < N; ++i) {
    if (!(i & interval)) {
      int un = 0;
      for (int i = 0; i < 256; ++i) {
        if (Table[i]) {
          ++un;
        }
      }
      if (un >= nmax) {
        break;
      }
    }
    unsigned int xpi = xp[i];
    Table[xpi] += 1;
  }
  SEXP ans = PROTECT(allocVector(INTSXP, 256));
  int * restrict ansp = INTEGER(ans);
  for (int i = 0; i < 256; ++i) {
    ansp[i] = Table[i];
  }
  UNPROTECT(1);
  return ans;
}

SEXP C_rank256(SEXP x, SEXP DoOrder) {
  if (isntRaw(x)) {
    return R_NilValue;
  }
  const bool do_order = asLogical(DoOrder);
  R_xlen_t N = xlength(x);
  const unsigned char * xp = RAW(x);
  unsigned int Table[256] = {0};
  do_tab256u(Table, xp, N);
  unsigned char xmin = 255;
  unsigned char xmax = 0;

  for (int i = 0; i < 256; ++i) {
    if (Table[i]) {
      xmin = i;
      break;
    }
  }
  for (int i = 255; i >= 0; --i) {
    if (Table[i]) {
      xmax = i;
      break;
    }
  }
  if (N >= INT_MAX) {
    return R_NilValue;
  }

  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  // for (int i = 0; i < N; ++i) {
  //   ansp[i] = -1;
  // }
  // if (N >= INT_MAX) {
  int k = 0;
  if (do_order) {
    for (int j = 1; j < 256; ++j) {
      Table[j] += Table[j - 1];
    }
    // unsigned int kposition[256] = {0};
    //
    // for (int j = 0; j < N; ++j) {
    //   unsigned int xpj = xp[j];
    //   if (!kposition[xpj]) {
    //     kposition[xpj] = j;
    //   }
    // }




    // p256[v + k * 256] is the position of the kth element of v
    // for (int j = 0; j < N; ++j) {
    //   unsigned int v = xp[j];
    //   unsigned int k256 = 256 * kposition[v];
    //   kposition[v]++;
    //   p256[v + k256] = j;
    // }
    //
    // int i = 0;
    // for (int v = 0; v < 256; ++v) {
    //   for (int k = 0; k < N; ++k) {
    //     unsigned int p256vk = p256[v + k * 256];
    //     if (p256vk == N) {
    //       break;
    //     }
    //     ansp[i] = p256vk + 1;
    //     ++i;
    //   }
    // }

    // From radixsort
    // o[--counts[(x[i] == NA_INTEGER) ? napos :
    // x[i] - xmin]] = (int) (i + 1);

    for (int i = N - 1; i >= 0; --i) {
      unsigned int xpi = xp[i];
      unsigned int Table_xpi = --Table[xpi];
      // if (Table_xpi >= N) {
      //   Rprintf("i = %d, Table_xpi = %d\n", i, Table_xpi);
      //   break;
      // }
      ansp[Table_xpi] = i + 1;
    }

    // for (int i = xmin; i <= xmax; ++i) {
    //   for (int j = 0; j < N; ++j) {
    //     if (!Table[i]) {
    //       break;
    //     }
    //     // if (k >= N) {
    //     //   break;
    //     // }
    //     if (xp[j] == i) {
    //       ansp[k] = j + 1;
    //       ++k;
    //       --Table[i];
    //     }
    //
    //   }
    // }
  } else {
    for (int i = xmin; i <= xmax; ++i) {
      for (int j = 0; j < N; ++j) {
        if (xp[j] == i) {
          ansp[j] = k + 1;
          ++k;
          // if (k >= N) {
          //   break;
          // }
        }
      }
      // if (k >= N) {
      //   break;
      // }
    }
  }
  UNPROTECT(1);
  return ans;

}




void bead_sort(int *a, int len) {
  int i, j, max, sum;
  unsigned char *beads;
#	define BEAD(i, j) beads[i * max + j]

  for (i = 1, max = a[0]; i < len; i++) {
    if (a[i] > max) max = a[i];
  }

  beads = calloc(1, max * len);

  /* mark the beads */
  for (i = 0; i < len; i++) {
    for (j = 0; j < a[i]; j++) {
      BEAD(i, j) = 1;
    }
  }

  for (j = 0; j < max; j++) {
    /* count how many beads are on each post */
    for (sum = i = 0; i < len; i++) {
      sum += BEAD(i, j);
      BEAD(i, j) = 0;
    }
    /* mark bottom sum beads */
    for (i = len - sum; i < len; i++) BEAD(i, j) = 1;
  }

  for (i = 0; i < len; i++) {
    for (j = 0; j < max && BEAD(i, j); j++);
    a[i] = j;
  }
  free(beads);
}

unsigned int interlace(unsigned char x, unsigned char y, unsigned char z, unsigned char w) {
  unsigned int o = x;
  o <<= 8;
  o += y;
  o <<= 8;
  o += z;
  o <<= 8;
  return o + w;
}

SEXP C_interlace256(SEXP x, SEXP y, SEXP z, SEXP w) {
  if (isntRaw(x) || isntRaw(y) || isntRaw(z) || isntRaw(w) ||
      xlength(x) != xlength(y) || xlength(x) != xlength(w) ||
      xlength(x) != xlength(z)) {
    return x;
  }
  R_xlen_t N = xlength(x);
  const unsigned char * xp = RAW(x);
  const unsigned char * yp = RAW(y);
  const unsigned char * zp = RAW(z);
  const unsigned char * wp = RAW(w);

  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);

  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = interlace(xp[i], yp[i], zp[i], wp[i]);
  }
  UNPROTECT(1);
  return ans;
}

SEXP C_deinterlace256(SEXP r) {
  if (!isInteger(r)) {
    return r;
  }
  const int * rp = INTEGER(r);
  R_xlen_t N = xlength(r);

  SEXP r0 = PROTECT(allocVector(RAWSXP, N));
  SEXP r1 = PROTECT(allocVector(RAWSXP, N));
  SEXP r2 = PROTECT(allocVector(RAWSXP, N));
  SEXP r3 = PROTECT(allocVector(RAWSXP, N));

  unsigned char * restrict r0p = RAW(r0);
  unsigned char * restrict r1p = RAW(r1);
  unsigned char * restrict r2p = RAW(r2);
  unsigned char * restrict r3p = RAW(r3);

  for (R_xlen_t i = 0; i < N; ++i) {
    unsigned int ri = rp[i];
    r3p[i] = ri;
    ri >>= 8;
    r2p[i] = ri;
    ri >>= 8;
    r1p[i] = ri;
    ri >>= 8;
    r0p[i] = ri;
  }
  SEXP ans = PROTECT(allocVector(VECSXP, 4));
  SET_VECTOR_ELT(ans, 0, r0);
  SET_VECTOR_ELT(ans, 1, r1);
  SET_VECTOR_ELT(ans, 2, r2);
  SET_VECTOR_ELT(ans, 3, r3);
  UNPROTECT(5);
  return ans;
}
