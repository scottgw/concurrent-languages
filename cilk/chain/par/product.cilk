/*
 * product: matrix-vector product
 *
 * input:
 *   nelts: the number of elements
 *   outer_matrix: a real matrix
 *   outer_vector: a real vector
 *
 * output:
 *   product_result: a real vector, whose values are the result of the product
 */

#include <cilk-lib.cilkh>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern double outer_matrix[10000][10000];
extern double outer_vector[10000];
double product_result[10000];

// parallel for on [begin, end)
cilk void fill_result(int begin, int end, int ncols) {
  int middle = begin + (end - begin) / 2;
  double sum = 0;
  int j;
  if (begin + 1 == end) {
    for (j = 0; j < ncols; j++) {
      sum += outer_matrix[begin][j] * outer_vector[j];
    }
    product_result[begin] = sum;
    return;
  }
  spawn fill_result(begin, middle, ncols);
  spawn fill_result(middle, end, ncols);
}

cilk void product(int nelts) {
  spawn fill_result(0, nelts, nelts);
}
