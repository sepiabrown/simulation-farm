#include <complex.h>
#include <inttypes.h>

typedef double complex TCD;
typedef float  complex TCF;

#undef complex

#include "lapack-aux.h"

#define V(x) x##n,x##p

#include <string.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define MACRO(B) do {B} while (0)
#define ERROR(CODE) MACRO(return CODE;)
#define REQUIRES(COND, CODE) MACRO(if(!(COND)) {ERROR(CODE);})
#define OK return 0;

#define MIN(A,B) ((A)<(B)?(A):(B))
#define MAX(A,B) ((A)>(B)?(A):(B))

#ifdef DBG
#define DEBUGMSG(M) printf("*** calling aux C function: %s\n",M);
#else
#define DEBUGMSG(M)
#endif

#define CHECK(RES,CODE) MACRO(if(RES) return CODE;)

#define BAD_SIZE 2000
#define BAD_CODE 2001
#define MEM      2002
#define BAD_FILE 2003


inline double urandom(struct random_data * buffer) {
    int32_t res;
    random_r(buffer,&res);
    return (double)res/RAND_MAX;
}


// http://c-faq.com/lib/gaussian.html
double gaussrand(struct random_data *buffer,
                 int *phase, double *pV1, double *pV2, double *pS)
{
	double V1=*pV1, V2=*pV2, S=*pS;
	double X;

	if(*phase == 0) {
		do {
            double U1 = urandom(buffer);
			double U2 = urandom(buffer);

			V1 = 2 * U1 - 1;
			V2 = 2 * U2 - 1;
			S = V1 * V1 + V2 * V2;
			} while(S >= 1 || S == 0);

		X = V1 * sqrt(-2 * log(S) / S);
	} else
		X = V2 * sqrt(-2 * log(S) / S);

	*phase = 1 - *phase;
    *pV1=V1; *pV2=V2; *pS=S;

	return X;

}

int random_vector(unsigned int seed, int code, DVEC(r)) {
    struct random_data buffer;
    char   random_state[128];
    memset(&buffer, 0, sizeof(struct random_data));
    memset(random_state, 0, sizeof(random_state));

    initstate_r(seed,random_state,sizeof(random_state),&buffer);
    // setstate_r(random_state,&buffer);
    // srandom_r(seed,&buffer);

    int phase = 0;
    double V1,V2,S;

    int k;
    switch (code) {
      case 0: { // uniform
        for (k=0; k<rn; k++) {
            rp[k] = urandom(&buffer);
        }
        OK
      }
      case 1: { // gaussian
        for (k=0; k<rn; k++) {
            rp[k] = gaussrand(&buffer,&phase,&V1,&V2,&S);
        }
        OK
      }

      default: ERROR(BAD_CODE);
    }
}
