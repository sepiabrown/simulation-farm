#include <immintrin.h>
#include <stddef.h>

void ewm_vectorized(double *a, double *b, size_t n) {
  int i;
  __m256d avec, bvec, resultvec;
  for (i = 0; i <= n - 4; i += 4) {
    // Load four elements from 'a' and 'b' into SIMD registers
    avec = _mm256_loadu_pd(&a[i]);
    bvec = _mm256_loadu_pd(&b[i]);
    // Square the elements of 'a'
    resultvec = _mm256_mul_pd(avec, avec);
    // Multiply 'b' with the squared elements of 'a'
    resultvec = _mm256_mul_pd(resultvec, bvec);
    // Store the result back into 'b'
    _mm256_storeu_pd(&b[i], resultvec);
  }
  // Handle the remainder of the array
  for (; i < n; ++i) {
    b[i] = a[i] * a[i] * b[i];
  }
}
