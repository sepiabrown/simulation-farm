#include <math.h>

double sum(int size, double *a) {
  double s = 0;
  for (int i=0; i < size; i++) {
    s += a[i];
  }
  return s;
}

double norm(int size, double *a) {
  double s = 0;
  for (int i=0; i < size; i++) {
    s += a[i]*a[i];
  }
  return sqrt(s);
}


double dot(int size, double *a, double *b) {
  double s = 0;
  for (int i=0; i < size; i++) {
    s += a[i]*b[i];
  }
  return s;
}


#define _GNU_SOURCE
#include <sys/types.h>
#include <sys/syscall.h>
#include <unistd.h>
#include <stdio.h>

unsigned get_ostid(void) {
  return syscall(SYS_gettid);
}
/* yes, I gamble that pid_t is essentially a word. */

void slow(unsigned n) {
  printf("slow sleeps in OS thread %u for %u seconds\n", get_ostid(), n);
  sleep(n);
}
