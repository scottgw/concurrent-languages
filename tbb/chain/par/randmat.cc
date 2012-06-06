/* randmat: random number generator
 *
 * input:
 *   nrows, ncols: number of rows and columns
 *   s: random number generation seed
 *
 * output:
 *   randmat_matrix: random nrows x ncols integer matrix
 */
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "tbb/parallel_for.h"
#include "tbb/blocked_range.h"
#include "tbb/task_scheduler_init.h"

using namespace tbb;

typedef blocked_range<size_t> range;

unsigned char randmat_matrix[30000][30000];

void randmat(int nrows, int ncols, unsigned int seed) {
  const int LCG_A = 1664525, LCG_C = 1013904223;
  parallel_for(
    range(0, nrows),
    [=, &seed](range r) {
      for (size_t i = r.begin(); i != r.end(); ++i) {
        for (int j = 0; j < ncols; j++) {
          randmat_matrix[i][j] = seed = (LCG_A * seed + LCG_C) % 100;
        }
      }
  });
}
