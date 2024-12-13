#include "stdio.h"
#include "stdlib.h"
#include "sys/time.h"
#include "time.h"

#ifdef _LP64
typedef int integer;
#else
typedef long int integer;
#endif

extern void dgemm_(char*, char*, integer*, integer*,integer*, double*, double*, integer*, double*, integer*, double*, double*, integer*);

int main(int argc, char* argv[])
{
  int i;
  printf("test!\n");
  if(argc<4){
    printf("Input Error\n");
    return 1;
  }

  integer m = atoi(argv[1]);
  integer n = atoi(argv[2]);
  integer k = atoi(argv[3]);
  integer sizeofa = m * k;
  integer sizeofb = k * n;
  integer sizeofc = m * n;
  char ta = 'N';
  char tb = 'N';
  double alpha = 1.2;
  double beta = 0.001;

  struct timeval start,finish;
  double duration;

  double* A = (double*)malloc(sizeof(double) * sizeofa);
  double* B = (double*)malloc(sizeof(double) * sizeofb);
  double* C = (double*)malloc(sizeof(double) * sizeofc);

  srand((unsigned)time(NULL));

  for (i=0; i<sizeofa; i++)
    A[i] = i%3+1;//(rand()%100)/10.0;

  for (i=0; i<sizeofb; i++)
    B[i] = i%3+1;//(rand()%100)/10.0;

  for (i=0; i<sizeofc; i++)
    C[i] = i%3+1;//(rand()%100)/10.0;
  //#if 0
  printf("m=%d,n=%d,k=%d,alpha=%lf,beta=%lf,sizeofc=%d\n",m,n,k,alpha,beta,sizeofc);
  gettimeofday(&start, NULL);
  dgemm_(&ta, &tb, &m, &n, &k, &alpha, A, &m, B, &k, &beta, C, &m);
  gettimeofday(&finish, NULL);

  duration = ((double)(finish.tv_sec-start.tv_sec)*1000000 + (double)(finish.tv_usec-start.tv_usec)) / 1000000;
  double gflops = 2.0 * m *n*k;
  gflops /= duration*1.0e-6;

  printf("%dx%dx%d\t%lf s\t%lf MFLOPS\n", m, n, k, duration, gflops);

  free(A);
  free(B);
  free(C);
  return 0;
}
