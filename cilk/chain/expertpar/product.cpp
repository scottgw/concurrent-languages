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

#include <cilk/cilk.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern double outer_matrix[10000][10000];
extern double outer_vector[10000];
double product_result[10000];

void product (int nelts) {
  cilk_for (int i = 0; i < nelts; ++i) {
    double sum = 0;
    for (int j = 0; j < nelts; ++j) {
      sum += outer_matrix [i][j] * outer_vector [j];
    }
    product_result [i] = sum;
  }
}
