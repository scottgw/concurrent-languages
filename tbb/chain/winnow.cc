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
#include "tbb/parallel_for.h"
#include "tbb/parallel_sort.h"

using namespace std;
using namespace tbb;

typedef blocked_range<size_t> range;

void winnow(int nrows, int ncols, const vector<vector<int> >& matrix,
    const vector<vector<int> >& mask, int nelts,
    vector<pair<int, int> >* points) {
  vector<pair<int, pair<int, int> > > values;

  parallel_for(
      range(0, nrows),
      [&](range r) {
        for (size_t i = r.begin(); i != r.end(); ++i) {
          tbb::parallel_for(
            range(0, ncols),
            [&](range s) {
              for (size_t j = s.begin(); j != s.end(); ++j) {
                if (mask[i][j]) {
                  values.push_back(make_pair(matrix[i][j],
                      make_pair(i, j)));
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

void read_matrix(int nrows, int ncols, vector<vector<int> >* matrix) {
  for (int i = 0; i < nrows; i++) {
    for (int j = 0; j < ncols; j++) {
      cin >> (*matrix)[i][j];
    }
  }
}

int main(int argc, char** argv) {
  int nrows, ncols, nelts;

  scanf("%d%d", &nrows, &ncols);

  vector<vector<int> > matrix(nrows, vector<int>(ncols));
  vector<vector<int> > mask(nrows, vector<int>(ncols));

  read_matrix(nrows, ncols, &matrix);
  read_matrix(nrows, ncols, &mask);

  scanf("%d", &nelts);

  vector<pair<int, int> > points(nelts);

  winnow(nrows, ncols, matrix, mask, nelts, &points);

  printf("%d\n", nelts);

  for (int i = 0; i < nelts; i++) {
    printf("%d %d\n", points[i].first, points[i].second);
  }
  printf("\n");

  return 0;
}
