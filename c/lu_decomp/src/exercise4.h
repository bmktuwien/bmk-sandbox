#ifndef _STL_SS14_EXERCISE4
#define _STL_SS14_EXERCISE4

#ifdef __cplusplus
extern "C" {
#endif 
#include "cblas.h" 
#ifdef __cplusplus  
} 
#endif

void lu ( const int n, double* A );
void lu_blas ( const int n, double* A );
void plu_block ( const int n, double* A, int* P);
double residual ( const int n, const double* A, const double* LU, const int* P, double* work );

#endif
