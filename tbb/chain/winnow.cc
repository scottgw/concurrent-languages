/* winnow: weighted point selection
 *
 * input:
 *   matrix: an integer matrix, whose values are used as masses
 *   mask: a boolean matrix, showing which points are eligible for
 *     consideration
 *   nrows, ncols: number of rows and columns
 *   nelts: the number of points to select
 *
 * output:
 *   points: a vector of (x, y) points
 */
#include <cassert>
#include <cstdio>
#include <cstdlib>

#include <algorithm>
#include <iostream>
#include <vector>

#include "tbb/blocked_range.h"
#include "tbb/mutex.h"
#include "tbb/parallel_for.h"
#include "tbb/parallel_sort.h"

using namespace std;
using namespace tbb;

typedef blocked_range<size_t> range;

void winnow(int nrows, int ncols, const vector<vector<int> >& matrix,
    const vector<vector<int> >& mask, int nelts,
    vector<pair<int, int> >* points) {
  vector<pair<int, pair<int, int> > > values;

  mutex m;

  parallel_for(
      range(0, nrows),
      [&](range r) {
        for (size_t i = r.begin(); i != r.end(); ++i) {
          tbb::parallel_for(
            range(0, ncols),
            [&](range s) {
              for (size_t j = s.begin(); j != s.end(); ++j) {
                if (mask[i][j]) {
                  m.lock();
                    values.push_back(make_pair(matrix[i][j],
                        make_pair(i, j)));
                  m.unlock();
                }
              }
            });
        }
      });

  parallel_sort(values.begin(), values.end());

  size_t n = values.size();
  size_t chunk = n / nelts;

  parallel_for(
      range(0, nelts),
      [&](range r) {
        for(size_t i = r.begin(); i != r.end(); ++i) {
          int index = i * chunk;
          (*points)[i] = values[index].second;
        }
      });
}
