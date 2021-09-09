#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP C_asInteger0(SEXP);
extern SEXP C_deinterlace256(SEXP);
extern SEXP C_interlace256(SEXP, SEXP, SEXP, SEXP);
extern SEXP C_nonDuplicated(SEXP, SEXP);
extern SEXP C_rank256(SEXP, SEXP);
extern SEXP Cchmatch256(SEXP);
extern SEXP Cfactor256_in(SEXP, SEXP, SEXP);
extern SEXP Cfactor2562logical(SEXP);
extern SEXP Cint2factor256(SEXP);
extern SEXP Clogical2factor256(SEXP);
extern SEXP CStackMatch(SEXP, SEXP);
extern SEXP Ctabulate256(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"C_asInteger0",       (DL_FUNC) &C_asInteger0,       1},
    {"C_deinterlace256",   (DL_FUNC) &C_deinterlace256,   1},
    {"C_interlace256",     (DL_FUNC) &C_interlace256,     4},
    {"C_nonDuplicated",    (DL_FUNC) &C_nonDuplicated,    2},
    {"C_rank256",          (DL_FUNC) &C_rank256,          2},
    {"Cchmatch256",        (DL_FUNC) &Cchmatch256,        1},
    {"Cfactor256_in",      (DL_FUNC) &Cfactor256_in,      3},
    {"Cfactor2562logical", (DL_FUNC) &Cfactor2562logical, 1},
    {"Cint2factor256",     (DL_FUNC) &Cint2factor256,     1},
    {"Clogical2factor256", (DL_FUNC) &Clogical2factor256, 1},
    {"CStackMatch",        (DL_FUNC) &CStackMatch,        2},
    {"Ctabulate256",       (DL_FUNC) &Ctabulate256,       1},
    {NULL, NULL, 0}
};

void R_init_factor256(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
