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

double *copy_sym_matrix(const double *M, size_t n)
{
    size_t i, j;
    double *T = malloc(n * n * sizeof(double));

    for (i = 0; i < n; i++) {
        for (j = i; j < n; j++) {
            T[i*n + j] = M[i*n + j];
            T[j*n + i] = M[i*n + j];
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
                /*printf("%d - %d: %f - %f\n", i, j, A[i*n + j], B[i*n + j]);*/
                return 0;
            }
        }
    }

    return 1;
}

void benchmark_matrix_func(matrix_func f, const char *name, size_t n)
{
    double *A = generate_random_matrix(n);
    double *B = generate_random_matrix(n);
    double *C = new_matrix(n);

    clock_t start = clock();
    (*f)(n, A, B, C);
    clock_t end = clock();

    printf("%s took %g secs with size %u\n", name, ((double) (end - start)) / CLOCKS_PER_SEC, (unsigned int) n);
}

double *reorder_matrix(const double *M, size_t n)
{
    size_t kk, jj, k, j, p=0;
    size_t m, l;
    size_t b = 300;
    double *T = malloc(n * n * sizeof(double));

    for (kk = 0; kk < n; kk += b) {
        for (jj = 0; jj < n; jj += b) {
            l = jj + b;
            if (l > n) l = n;

            for (j = jj; j < l; j++) {
                m = kk + b;
                if (m > n) m = n;

                for (k = kk; k < m; k++) {
                    T[p] = M[k*n + j];
                    p++;
                }
            }
        }
    }

    return T;
}

void matrixMatrix(const int n, const double *A, const double *B, double *C)
{
    size_t kk, jj, i, k, j, p=0, q=0;
    size_t m, l;
    size_t b = 300;
    double *ROB = reorder_matrix(B, n);


    for (kk = 0; kk < n; kk += b) {
        for (jj = 0; jj < n; jj += b) {
            for (i = 0; i < n; i++) {
                q=0;
                l = jj + b;
                if (l > n) l = n;

                for (j = jj; j < l; j++) {
                    register double r = C[i*n + j];

                    m = kk + b;
                    if (m > n) m = n;

                    for (k = kk; k < m; k++) {
                        r += A[i*n + k] * ROB[p];
                        p++;
                        q++;
                    }

                    C[i*n + j] = r;
                }

                if (i < n -1)
                    p -= q;
            }
        }
    }

    free(ROB);
}

void symmatrixMatrix(const int n, const double *A, const double *B, double *C)
{
    double *SA = copy_sym_matrix(A, n);

    matrixMatrix(n, SA, B, C);

    free(SA);
}

void matrixMatrix2(const int n, const double *A, const double *B, double *C)
{
    size_t i, k, j;
    register double r;
    double *TB = transpose_matrix(B, n);

    for (i = 0; i < n; i++) {
        for (j = 0; j < n; j++) {
            r = C[i*n + j];

            for (k = 0; k < n; k++) {
                r += A[i*n + k] * TB[j*n + k];
            }

            C[i*n + j] = r;
        }
    }

    free(TB);
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

void test_matrixMatrix(size_t n)
{
    double *A = generate_random_matrix(n);
    double *B = generate_random_matrix(n);
    double *C = new_matrix(n);
    double *D = new_matrix(n);

    matrixMatrix(n, A, B, C);
    naivematrixMatrix(n, A, B, D);

    if (memcmp(C, D, n * n * sizeof(double)) != 0) {
        printf("matrixMatrix test failed!\n");
    } else {
        printf("matrixMatrix test success!\n");
    }
}

void test_symmatrixMatrix(size_t n)
{
    double *A = generate_random_matrix(n);
    double *B = generate_random_matrix(n);
    double *C = new_matrix(n);
    double *D = new_matrix(n);

    symmatrixMatrix(n, A, B, C);
    naivematrixMatrix(n, copy_sym_matrix(A, n), B, D);

    if (memcmp(C, D, n * n * sizeof(double)) != 0) {
        printf("symmatrixMatrix test failed!\n");
    } else {
        printf("symmatrixMatrix test success!\n");
    }
}

int main(int argc, const char* argv[])
{
    srand (time(NULL));

    benchmark_matrix_func(matrixMatrix, "matrixMatrix", 2000);
    benchmark_matrix_func(symmatrixMatrix, "symmatrixMatrix", 2000);

    size_t s;
    for (s = 100; s <= 1000; s += 100) {
        test_matrixMatrix(s);
        test_symmatrixMatrix(s);
    }

    return 0;
}