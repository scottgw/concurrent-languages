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

extern double outer_matrix[10000][10000];
extern double outer_vector[10000];
double product_result[10000];

typedef blocked_range<size_t> range;

void product(int nelts) {
  parallel_for(
    range(0, nelts),
    [&](range r) {
      for (size_t i = r.begin(); i != r.end(); ++i) {
        int j = 0;
        double sum = 0;

        for (; (nelts - j) & 3; ++j)
          sum  += matrix [i][j]     * vec [j];

        double acc1, acc2, acc3, acc4;
        acc1 = acc2 = acc3 = acc4 = 0;

        for (; j < nelts; j += 4) {
          acc1 += matrix [i][j]     * vec [j];
          acc2 += matrix [i][j + 1] * vec [j + 1];
          acc3 += matrix [i][j + 2] * vec [j + 2];
          acc4 += matrix [i][j + 3] * vec [j + 3];
        }

        sum += acc1 + acc2 + acc3 + acc4;

        result [i] = sum;
      }
  });
}
