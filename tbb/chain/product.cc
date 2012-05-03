/* product: matrix-vector product
 *
 * input:
 *   matrix: a real matrix
 *   vec: a real vector
 *   nelts: the number of elements
 *
 * output:
 *   result: a real vector, whose values are the result of the product
 */
#include <cstdio>

#include <iostream>
#include <vector>

#include "tbb/blocked_range.h"
#include "tbb/parallel_for.h"

using namespace std;
using namespace tbb;

typedef blocked_range<size_t> range;

void product(int nelts, const vector<vector<double> >& matrix,
    const vector<double>& vec, vector<double>* result) {
  parallel_for(
    range(0, nelts),
    [&](range r) {
      for (size_t i = r.begin(); i != r.end(); ++i) {
        double sum = 0;
        for (int j = 0; j < nelts; j++) {
          sum += matrix[i][j] * vec[j];
        }
        (*result)[i] = sum;
      }
  });
}

