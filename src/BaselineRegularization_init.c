#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP bflsa(SEXP, SEXP, SEXP);
extern SEXP bwflsa(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"bflsa",  (DL_FUNC) &bflsa,  3},
  {"bwflsa", (DL_FUNC) &bwflsa, 4},
  {NULL, NULL, 0}
};

void R_init_BaselineRegularization(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
