/* winnow: weighted point selection
 *
 * input:
 *   randmat_matrix: an integer matrix, whose values are used as masses
 *   thresh_mask: a boolean matrix, showing which points are eligible for
 *     consideration
 *   nrows, ncols: number of rows and columns
 *   nelts: the number of points to select
 *
 * output:
 *   winnow_points: a vector of (x, y) points
 */
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include <algorithm>
#include <iostream>
#include <vector>

using namespace std;

extern int is_bench;
extern unsigned char randmat_matrix[20000][20000];
extern unsigned char thresh_mask[20000][20000];
pair<int, int> winnow_points[20000];
static pair<int, pair<int, int> > values[20000];

void winnow(int nrows, int ncols, int nelts) {
  int count = 0;
  for (int i = 0; i < nrows; i++) {
    for (int j = 0; j < ncols; j++) {
      if (is_bench) {
        thresh_mask[i][j] = ((i * j) % (ncols + 1)) == 1;
      }
      if (thresh_mask[i][j]) {
        values[count++] = (make_pair(randmat_matrix[i][j], make_pair(i, j)));
      }
    }
  }

  sort(values, values + count);

  size_t n = count;
  size_t chunk = n / nelts;

  for (int i = 0; i < nelts; i++) {
    int index = i * chunk;
    winnow_points[i] = values[index].second;
  }
}
