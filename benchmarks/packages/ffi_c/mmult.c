#include <stdio.h>

void mmult(const double *a, const double *b, double *res, const int n, const int m, const int p) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < p; j++) {
            res[i * p + j] = 0;
            for (int k = 0; k < m; k++) {
                res[i * p + j] += a[i * m + k] * b[p * k + j];
            }
        }
    }
}
