#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

typedef void (*matrix_func)(const int n, const double *A, const double *B, double *C);

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

int equals(double *A, double *B, size_t n)
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

void naivematrixMatrix(const int n, const double *A, const double *B, double *C)
{
    size_t i, j, k;

    for (i = 0; i < n; i++) {
        for (j = 0; j < n; j++) {
            for (k = 0; k < n; k++) {
                C[i*n + j] += A[i*n + k] * B[k*n + j];
            }
        }
    }

}

void lu(const int n, double* A)
{

    int i, j, k, p;

    for (k = 0; k < n; k++) {
        for (j = k; j < n; j++) {
            for (p = 0; p < k; p++)
                A[k*n + j] -= A[k*n + p] * A[p*n + j];
        }

        for (i = k+1; i < n; i++) {
            for (p = 0; p < k; p++)
                A[i*n + k] -= A[i*n + p] * A[p*n + k];
            A[i*n + k] /= A[k*n + p];
        }
    }


    print_matrix(A, n);

}


void lu_reference(const int n, double* A)
{
    int i, j, k, p;
    double *p_k, *p_row, *p_col;

    //         For each row and column, k = 0, ..., n-1,
    //            find the upper triangular matrix elements for row k
    //            and if the matrix is non-singular (nonzero diagonal element).
    //            find the lower triangular matrix elements for column k.

    for (k = 0, p_k = A; k < n; p_k += n, k++) {
        for (j = k; j < n; j++) {
            for (p = 0, p_col = A; p < k; p_col += n,  p++)
                *(p_k + j) -= *(p_k + p) * *(p_col + j);
        }

        for (i = k+1, p_row = p_k + n; i < n; p_row += n, i++) {
            for (p = 0, p_col = A; p < k; p_col += n, p++)
                *(p_row + k) -= *(p_row + p) * *(p_col + k);
            *(p_row + k) /= *(p_k + k);
        }
    }

    print_matrix(A, n);
}


int main(int argc, const char* argv[])
{
    srand (time(NULL));

    /*
      1   3   5
      2   4   7
      1   1   0
    */
    double *A = new_matrix(3);
    A[0] = 1; A[1] = 3; A[2] = 5;
    A[3] = 2; A[4] = 4; A[5] = 7;
    A[6] = 1; A[7] = 1; A[8] = 0;

    lu(3, A);


    return 0;
}
