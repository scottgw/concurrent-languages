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

#include "tbb/parallel_for.h"
#include "tbb/parallel_reduce.h"
#include "tbb/blocked_range.h"

using namespace std;

typedef tbb::blocked_range<size_t> range;
typedef function<int (int, int)> Operator;

int op_max(int x, int y) {
  return max(x, y);
}

int op_sum(int x, int y) {
  return x + y;
}

int reduce2d_with_filter(int nrows, int ncols,
    const vector<vector<int>>& matrix, Operator aggregator, Operator op) {
  return tbb::parallel_reduce(
      range(0, nrows), 0,
      [=](range r, int partial_value)->int {
        int result = partial_value;
        for (size_t i = r.begin(); i != r.end(); i++) {
          result = aggregator(result, tbb::parallel_reduce(
            range(0, nrows), 0,
            [=](range r, int partial_value)->int {
              int result = partial_value;
              for (size_t j = r.begin(); j != r.end(); j++) {
                result = op(result, matrix[i][j]);
              }
              return result;
            },
            aggregator));
        }
        return result;
      },
      aggregator);
}

int reduce2d(int nrows, int ncols, const vector<vector<int>>& matrix,
    Operator op) {
  return reduce2d_with_filter(nrows, ncols, matrix, op, op);
}

void thresh(int nrows, int ncols, const vector<vector<int>>& matrix,
    int percent, vector<vector<int>>* mask) {

  int nmax = reduce2d(nrows, ncols, matrix, op_max);

  vector<int> histogram(nmax + 1, 0);

  tbb::parallel_for(
      range(0, nrows),
      [&](range r) {
        for (size_t i = r.begin(); i != r.end(); ++i) {
          histogram[i] = reduce2d_with_filter(nrows, ncols, matrix, op_sum,
            [=](int acc, int value) {
              printf("sum[%d](%d, %d)\n", i, acc, value);
              return acc + (value == i);
            });
        }
      });

  for (int i = 0; i < nmax + 1; i++) {
    printf("%d ", histogram[i]);
  }
  printf("\n");

  //for (int i = 0; i < nrows; i++) {
    //for (int j = 0; j < ncols; j++) {
      //histogram[matrix[i][j]]++;
    //}
  //}

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
