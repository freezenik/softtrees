#include <stdio.h>
#include <math.h>
#include <assert.h>
#include <stdlib.h>
#include <time.h>
// #include <omp.h>

#include <R.h>
#include <Rmath.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include <R_ext/Applic.h> /* for dgemm */
#include <R_ext/Complex.h>
#include <R_ext/RS.h>
#include <R_ext/BLAS.h>
#include <R_ext/Lapack.h>
#include <R_ext/Linpack.h>

/* Process derivatives. */
SEXP process_derivs(SEXP x, SEXP w)
{
  int i;
  int n = length(x);

  SEXP rval;
  PROTECT(rval = allocVector(REALSXP, n));

  double *xptr = REAL(x);
  double *rvalptr = REAL(rval);

  for(i = 0; i < n; i++) {
    rvalptr[i] = xptr[i];
    if(ISNA(xptr[i]))
      rvalptr[i] = 1.490116e-08;
    if(ISNAN(xptr[i]))
      rvalptr[i] = 1.490116e-08;
    if(xptr[i] > 1e+10)
      rvalptr[i] = 1e+10;
    if(LOGICAL(w)[0]) {
      if(xptr[i] == 0.0)
        rvalptr[i] = 1.490116e-08;
      if(xptr[i] < 0.0)
        rvalptr[i] = -1.0 * xptr[i];
    } else {
      if(xptr[i] < -1e+10)
        rvalptr[i] = -1e+10;
    }
  }

  UNPROTECT(1);

  return rval;
}

