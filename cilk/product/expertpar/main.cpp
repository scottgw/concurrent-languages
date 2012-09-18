/*
 * product: matrix-vector product
 *
 * input:
 *   nelts: the number of elements
 *   matrix: a real matrix
 *   vector: a real vector
 *
 * output:
 *   result: a real vector, whose values are the result of the product
 */

#include <cilk/cilk.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int is_bench = 0;

static double matrix[10000][10000];
static double vector[10000];
static double result[10000];

// parallel for on [begin, end)
void product (int nelts) {
  cilk_for (int i = 0; i < nelts; ++i) {
    double sum = 0;
    for (int j = 0; j < nelts; ++j) {
      sum += matrix [i][j] * vector [j];
    }
    result [i] = sum;
  }
}

void read_matrix(int nelts) {
  int i, j;
  for (i = 0; i < nelts; i++) {
    for (j = 0; j < nelts; j++) {
      scanf("%lf", &matrix[i][j]);
    }
  }
}

void read_vector(int nelts) {
  int i;
  for (i = 0; i < nelts; i++) {
    scanf("%lf", &vector[i]);
  }
}

int main(int argc, char *argv[]) {
  int nelts, i;

  if (argc == 2) {
    if (!strcmp(argv[argc - 1], "--is_bench")) {
      is_bench = 1;
    }
  }

  scanf("%d", &nelts);
  if (!is_bench) {
    read_matrix(nelts);
    read_vector(nelts);
  }

  product(nelts);

  if (!is_bench) {
    printf("%d\n", nelts);
    for (i = 0; i < nelts; i++) {
      printf("%g ", result[i]);
    }
    printf("\n");
  }

  return 0;
}
