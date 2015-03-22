#ifndef _STL_SS14_EXERCISE6
#define _STL_SS14_EXERCISE6

#ifdef __cplusplus
extern "C" {
#endif 
#include "cblas.h" 
#include "clapack.h" 
#ifdef __cplusplus
} 
#endif

#include "plasma.h"
#include "common.h"

void stl_plasma ( const int function, const int n, const double *A, const double *B, double *X );

double residual (  const int n, const double* A, const double* B, const double* X );

#endif
