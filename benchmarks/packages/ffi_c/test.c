#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int main() {
  size_t n = 10000000;
  double* arr = (double*) malloc (n*sizeof(double));

  srand(1);

  for (int i=0; i<(int)n; i++) {
    arr[i] = (double) rand() / RAND_MAX;
  }

  double b = 1.0;
  double a = log(b);

  printf("a is %f\n", a);


  free(arr);
}
