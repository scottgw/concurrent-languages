/* product: matrix-vector product
 *
 * input:
 *   outer_matrix: a real matrix
 *   outer_vector: a real vector
 *   nelts: the number of elements
 *
 * output:
 *   product_result: a real vector, whose values are the result of the product
 */
#include <cstdio>
#include <cstring>

#include <iostream>
#include <vector>

#include "tbb/blocked_range.h"
#include "tbb/parallel_for.h"
#include "tbb/task_scheduler_init.h"

using namespace std;
using namespace tbb;

extern double *outer_matrix;
extern double *outer_vector;
double *product_result;

typedef blocked_range<size_t> range;

void product(int nelts) {
  product_result = (double*) malloc (sizeof(double) * nelts);
  parallel_for(
    range(0, nelts),
    [&](range r) {
      for (size_t i = r.begin(); i != r.end(); ++i) {
        int j = 0;
        double sum = 0;

        for (; (nelts - j) & 3; ++j)
          sum  += outer_matrix [i*nelts + j]     * outer_vector [j];

        double acc1, acc2, acc3, acc4;
        acc1 = acc2 = acc3 = acc4 = 0;

        for (; j < nelts; j += 4) {
          acc1 += outer_matrix [i*nelts + j]     * outer_vector [j];
          acc2 += outer_matrix [i*nelts + j + 1] * outer_vector [j + 1];
          acc3 += outer_matrix [i*nelts + j + 2] * outer_vector [j + 2];
          acc4 += outer_matrix [i*nelts + j + 3] * outer_vector [j + 3];
        }

        sum += acc1 + acc2 + acc3 + acc4;

        product_result [i] = sum;
      }
  });
}
