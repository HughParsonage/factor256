#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP BSearch(SEXP, SEXP);
extern SEXP C_asInteger0(SEXP);
extern SEXP Cfactor256_in(SEXP, SEXP, SEXP);
extern SEXP Cint2factor256(SEXP);
extern SEXP Clogical2factor256(SEXP);
extern SEXP CStackMatch(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"BSearch",            (DL_FUNC) &BSearch,            2},
    {"C_asInteger0",       (DL_FUNC) &C_asInteger0,       1},
    {"Cfactor256_in",      (DL_FUNC) &Cfactor256_in,      3},
    {"Cint2factor256",     (DL_FUNC) &Cint2factor256,     1},
    {"Clogical2factor256", (DL_FUNC) &Clogical2factor256, 1},
    {"CStackMatch",        (DL_FUNC) &CStackMatch,        2},
    {NULL, NULL, 0}
};

void R_init_factor256(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
