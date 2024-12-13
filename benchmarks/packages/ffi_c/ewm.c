#include <stdio.h>

void ewm(int n, const double *a, double *b) {
  for (int i=0; i<(int)n; i++) {
    b[i] *= a[i];
  }
}
