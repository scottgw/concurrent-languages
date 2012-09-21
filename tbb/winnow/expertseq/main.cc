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
#include <cstring>

#include <algorithm>
#include <iostream>
#include <vector>

using namespace std;

int is_bench = 0;
static unsigned char matrix[20000][20000];
static unsigned char mask[20000][20000];
static pair<int, int> points[20000];
static pair<int, pair<int, int> > values[20000];

void winnow(int nrows, int ncols, int nelts) {
  int count = 0;
  for (int i = 0; i < nrows; i++) {
    for (int j = 0; j < ncols; j++) {
      if (is_bench) {
        mask[i][j] = ((i * j) % (ncols + 1)) == 1;
      }
      if (mask[i][j]) {
        values[count++] = (make_pair(matrix[i][j], make_pair(i, j)));
      }
    }
  }

  sort(values, values + count);

  size_t n = count;
  size_t chunk = n / nelts;

  for (int i = 0; i < nelts; i++) {
    int index = i * chunk;
    points[i] = values[index].second;
  }
}

void read_matrix(int nrows, int ncols) {
  for (int i = 0; i < nrows; i++) {
    for (int j = 0; j < ncols; j++) {
      int v;
      cin >> v;
      matrix[i][j] = v;
    }
  }
}

void read_mask(int nrows, int ncols) {
  for (int i = 0; i < nrows; i++) {
    for (int j = 0; j < ncols; j++) {
      int v;
      cin >> v;
      mask[i][j] = v;
    }
  }
}

int main(int argc, char** argv) {
  int nrows, ncols, nelts;

  for (int i = 1; i < argc; i++) {
    if (!strcmp(argv[i], "--is_bench")) {
      is_bench = 1;
    }
  }

  scanf("%d%d", &nrows, &ncols);

  if (!is_bench) {
    read_matrix(nrows, ncols);
    read_mask(nrows, ncols);
  }

  scanf("%d", &nelts);

  winnow(nrows, ncols, nelts);

  if (!is_bench) {
    printf("%d\n", nelts);

    for (int i = 0; i < nelts; i++) {
      printf("%d %d\n", points[i].first, points[i].second);
    }
    printf("\n");
  }

  return 0;
}
