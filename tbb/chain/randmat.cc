/* randmat: random number generator
 *
 * input:
 *   nrows, ncols: number of rows and columns
 *   s: random number generation seed
 *
 * output:
 *   matrix: random nrows x ncols integer matrix
 */
#include <cstdlib>

#include <vector>

#include "tbb/parallel_for.h"
#include "tbb/blocked_range.h"

using namespace std;
using namespace tbb;

typedef tbb::blocked_range<size_t> range;

void randmat(int nrows, int ncols, int s, vector<vector<int> >* matrix) {
  srand(s);
  parallel_for(
      range(0, nrows),
      [&](range r) {
        for (size_t i = r.begin(); i != r.end(); ++i) {
          for (int j = 0; j < ncols; j++) {
            (*matrix)[i][j] = rand() % 1000;
          }
        }
      });
}

