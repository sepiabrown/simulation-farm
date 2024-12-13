#include <stdio.h>
#include <cblas.h>


#ifdef _LP64
typedef int integer;
#else
typedef long int integer;
#endif

/* void dgemm_(char *, char *, integer *, integer *, integer *, */
/*            double *, const double *, integer *, */
/*            const double *, integer *, */
/*            double *, double *, integer *); */


/* void dgemm(integer m, integer n, integer k, */
/*           const double *a, integer lda, */
/*           const double *b, integer ldb, */
/*           double *c, integer ldc) { */
/*     double alpha = 1; */
/*     double beta = 0; */
/*     dgemm_("N", "N", &m, &n, &k, &alpha, a, &lda, b, &ldb, &beta, c, &ldc); */
/* } */

void dgemm(integer m, integer n, integer k,
          const double *a, const double *b, double *c) {
    double alpha = 1;
    double beta = 0;
    cblas_dgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, m, n, k, alpha, a, k, b, n, beta, c, n);
}
