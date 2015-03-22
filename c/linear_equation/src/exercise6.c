#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "exercise6.h"


double *generate_random_matrix(size_t n)
{
    size_t i;
    double *matrix = malloc(sizeof(double) * n * n);

    for (i = 0; i < n*n; i++) {
        matrix[i] = (double) (rand() % 100);
    }

    return matrix;
}

double *new_matrix(size_t n)
{
    return calloc(n * n, sizeof(double));
}

double *transpose_matrix(const double *M, size_t n)
{
    size_t i, j;
    double *T = malloc(n * n * sizeof(double));

    for (i = 0; i < n; i++) {
        for (j = 0; j < n; j++) {
            T[i*n + j] = M[j*n + i];
        }
    }

    return T;
}

double *copy_matrix(const double *M, size_t n)
{
    size_t i, j;
    double *T = malloc(n * n * sizeof(double));

    for (i = 0; i < n; i++) {
        for (j = 0; j < n; j++) {
            T[i*n + j] = M[i*n + j];
        }
    }

    return T;
}

void copy_to(const double *M, double *A, const size_t n)
{
    size_t i, j;

    for (i = 0; i < n; i++) {
        for (j = 0; j < n; j++) {
            A[i*n + j] = M[i*n + j];
        }
    }
}

void print_matrix(double *m, size_t n)
{
    size_t i, j;

    for (i = 0; i < n; i++) {
        for (j = 0; j < n; j++) {
            printf("%f ", m[i*n + j]);
        }
        printf("\n");
    }
}

int matrix_equals(double *A, double *B, size_t n)
{
    size_t i, j;

    for (i = 0; i < n; i++) {
        for (j = 0; j < n; j++) {
            if (A[i*n + j] != B[i*n + j]) {
                return 0;
            }
        }
    }

    return 1;
}


void bail_out(const char *err_msg)
{
    fprintf(stderr, "fata error: %s\n", err_msg);
    exit(2);
}


/*
int check_solution(int N, int NRHS, double *A1, int LDA, double *B1, double *B2, int LDB)
{
    int info_solution;
    double Rnorm, Anorm, Xnorm, Bnorm;
    double alpha, beta;
    double *work = (double *) malloc(N*sizeof(double));
    double eps;

    eps = LAPACKE_dlamch_work('e');

    alpha = 1.0;
    beta  = -1.0;

    Xnorm = LAPACKE_dlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), N, NRHS, B2, LDB, work);
    Anorm = LAPACKE_dlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), N, N, A1, LDA, work);
    Bnorm = LAPACKE_dlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), N, NRHS, B1, LDB, work);

    cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, N, NRHS, N, (alpha), A1, LDA, B2, LDB, (beta), B1, LDB);
    Rnorm = LAPACKE_dlange_work(LAPACK_COL_MAJOR, lapack_const(PlasmaInfNorm), N, NRHS, B1, LDB, work);

    printf("============\n");
    printf("Checking the Residual of the solution \n");
    printf("-- ||Ax-B||_oo/((||A||_oo||x||_oo+||B||_oo).N.eps) = %e \n",Rnorm/((Anorm*Xnorm+Bnorm)*N*eps));

    if ( isnan(Rnorm/((Anorm*Xnorm+Bnorm)*N*eps)) || (Rnorm/((Anorm*Xnorm+Bnorm)*N*eps) > 10.0) ){
        printf("-- The solution is suspicious ! \n");
        info_solution = 1;
    }
    else{
        printf("-- The solution is CORRECT ! \n");
        info_solution = 0;
    }

    free(work);

    return info_solution;
}
*/

void solve_equation_system_1(const int N, const double *A, const double *B, double *X)
{
    int info;
    int cores = 2;

    info = PLASMA_Init(cores);
    if (info < 0) {
        bail_out("Plasma init failed");
    }

    /* allocate the work space */
    PLASMA_desc *L;
    int LDA = N;
    int LDB = N;
    int NRHS = N;
    int *IPIV;
    double *AW = copy_matrix(A, N);
    info = PLASMA_Alloc_Workspace_dgesv_incpiv(N, &L, &IPIV);
    if (info != 0) {
        bail_out("PLASMA_ALLOC_WORKSPACE_dgesv call failed");
    }

    /* copy the matrix B into the output matrix X */
    copy_to(B, X, N);

    /* solve the equation */
    info = PLASMA_dgesv_incpiv(N, NRHS, AW, LDA, L, IPIV, X, LDB);
    if (info != 0) {
        bail_out("PLASMA_degesv call failed");
    }

    /* cleanup */
    free(IPIV);
    free(L);
    free(AW);

    PLASMA_Finalize();
}

void solve_equation_system_2(const int N, const double *A, const double *B, double *X)
{
    int info;
    int cores = 2;

    info = PLASMA_Init(cores);
    if (info < 0) {
        bail_out("Plasma init failed");
    }

    /* allocate the work space */
    PLASMA_desc *L;
    int LDA = N;
    int LDB = N;
    int NRHS = N;
    int *IPIV;
    double *AW = copy_matrix(A, N);
    info = PLASMA_Alloc_Workspace_dgetrf_incpiv(N, N, &L, &IPIV);
    if (info != 0) {
        bail_out("PLASMA_ALLOC_WORKSPACE_dgetrf call failed");
    }

    /* copy the matrix B into the output matrix X */
    copy_to(B, X, N);

    /* LU factorization of the matrix A */
    info = PLASMA_dgetrf_incpiv(N, N, AW, LDA, L, IPIV);
    if (info != 0) {
        bail_out("PLASMA_dgetrf call failed");
    }

    /* Solve the problem */
    info = PLASMA_dgetrs_incpiv(PlasmaNoTrans, N, NRHS, AW, LDA, L, IPIV, X, LDB);
    if (info != 0) {
        bail_out("PLASMA_dgetrs call failed");
    }

    /* cleanup */
    free(IPIV);
    free(L);
    free(AW);

    PLASMA_Finalize();
}

void stl_plasma (const int function, const int n, const double *A, const double *B, double *X)
{
    switch(function) {
    case 1:
        solve_equation_system_1(n, A, B, X);
        break;
    case 2:
        solve_equation_system_2(n, A, B, X);
        break;
    default:
        /* other functions are not implemented yet */
        break;
    }
}


void testrun_solvers(int n) {
    double *A = generate_random_matrix(n);
    double *B = generate_random_matrix(n);
    double *X1 = new_matrix(n);
    double *X2 = new_matrix(n);

    //print_matrix(A, n);
    //printf("-----------------------------------\n");
    //print_matrix(B, n);
    solve_equation_system_1(n, A, B, X1);
    solve_equation_system_1(n, A, B, X2);

    //printf("===================================\n");
    //print_matrix(X1, n);
    //printf("***********************************\n");
    //print_matrix(X2, n);

    if (!matrix_equals(X1, X2, n)) {
        bail_out("Results of the sovler1 and solver2 are not equal");
    } else {
        printf("SUCCESS: Results of the solver1 and sovler2 are equal\n");
    }

    free(A);
    free(B);
    free(X1);
    free(X2);
}


int main(int argc, const char* argv[])
{
    srand (time(NULL));

    testrun_solvers(5);

    return 0;
}
