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
        double sum = 0;
        for (int j = 0; j < nelts; j++) {
          sum += outer_matrix[i][j] * outer_vector[j];
        }
        product_result[i] = sum;
      }
  });
}
