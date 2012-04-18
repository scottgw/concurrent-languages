/* thresh: histogram thresholding
 *
 * input:
 *   matrix: the integer matrix to be thresholded
 *   nrows, ncols: number of rows and columns
 *   percent: the percentage of cells to retain
 *
 * output:
 *   mask: a boolean matrix filled with true for cells that are kept
 */
#include <cassert>
#include <cstdio>
#include <cstdlib>

#include <algorithm>

#include "tbb/parallel_reduce.h"
#include "tbb/blocked_range.h"

using namespace std;

typedef tbb::blocked_range<size_t> range;

struct sOpMax{
  int operator()(const int& x, const int& y) const {
    return max(x, y);
  }
};

template <class Operator>
int reduce2d(int nrows, int ncols, const vector<vector<int>>& matrix,
    Operator op) {
  return tbb::parallel_reduce(
      range(0, nrows), 0,
      [=](range r, int partial_value)->int {
        int result = partial_value;
        for (size_t i = r.begin(); i != r.end(); i++) {
          result = op(result, tbb::parallel_reduce(
            range(0, nrows), 0,
            [=](range r, int partial_value)->int {
              int result = partial_value;
              for (size_t j = r.begin(); j != r.end(); j++) {
                result = op(result, matrix[i][j]);
              }
              return result;
            },
            op));
        }
        return result;
      },
      op);
}

void thresh(int nrows, int ncols, const vector<vector<int>>& matrix,
    int percent, vector<vector<int>>* mask) {

  int nmax = reduce2d(nrows, ncols, matrix, sOpMax());

  vector<int> histogram(nmax + 1, 0);

  for (int i = 0; i < nrows; i++) {
    for (int j = 0; j < ncols; j++) {
      histogram[matrix[i][j]]++;
    }
  }

  int count = (nrows * ncols * percent) / 100;

  int prefixsum = 0;
  int threshold = nmax;

  for (int i = nmax; i >= 0 && prefixsum <= count; i--) {
    prefixsum += histogram[i];
    threshold = i;
  }

  printf("threshold: %d\n", threshold);

  for (int i = 0; i < nrows; i++) {
    for (int j = 0; j < ncols; j++) {
      (*mask)[i][j] = matrix[i][j] >= threshold;
    }
  }
}

int main(int argc, char** argv) {
  int nrows, ncols, percent;

  scanf("%d%d", &nrows, &ncols);

  vector<vector<int>> matrix(nrows, vector<int>(ncols));
  vector<vector<int>> mask(nrows, vector<int>(ncols));

  for (int i = 0; i < nrows; i++) {
    for (int j = 0; j < ncols; j++) {
      scanf("%d", &matrix[i][j]);
    }
  }

  scanf("%d", &percent);

  thresh(nrows, ncols, matrix, percent, &mask);

  printf("%d %d\n", nrows, ncols);
  for (int i = 0; i < nrows; i++) {
    for (int j = 0; j < ncols; j++) {
      printf("%d ", mask[i][j]);
    }
    printf("\n");
  }
  printf("\n");

  return 0;
}
