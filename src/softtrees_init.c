#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP process_derivs(SEXP, SEXP);

static R_CallMethodDef callMethods[] = {
  {"process_derivs", (DL_FUNC) &process_derivs, 2},
  {NULL, NULL, 0}
};

void R_init_sourcetools(DllInfo* info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}

