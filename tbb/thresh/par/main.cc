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

#include "tbb/blocked_range.h"
#include "tbb/parallel_for.h"
#include "tbb/parallel_reduce.h"
#include "tbb/parallel_scan.h"

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
          int temp = 0;
          for (int j = 0; j < ncols; j++) {
            temp = op(temp, matrix[i][j]);
          }
          result = aggregator(result, temp);
        }
        return result;
      },
      aggregator);
}

int reduce2d(int nrows, int ncols, const vector<vector<int>>& matrix,
    Operator op) {
  return reduce2d_with_filter(nrows, ncols, matrix, op, op);
}

class ScanSum {
  int sum;
  vector<int>* y;
  const vector<int>& x;

public:
  ScanSum(vector<int>* y_, const vector<int>& x_): sum(0), x(x_), y(y_) {}
  template<typename Tag>
  void operator()(range r, Tag) {
    int res = sum;
    for (size_t i = r.begin(); i != r.end(); ++i) {
      res += x[i];
      if (Tag::is_final_scan()) {
        (*y)[i] = res;
      }
    }
    sum = res;
  }
  ScanSum(ScanSum& other, tbb::split) : x(other.x), y(other.y), sum(0) {}
  void reverse_join(ScanSum& other) { sum += other.sum; }
  void assign(ScanSum& other) { sum = other.sum; }
};

void thresh(int nrows, int ncols, const vector<vector<int>>& matrix,
    int percent, vector<vector<int>>* mask) {

  int nmax = reduce2d(nrows, ncols, matrix, op_max);

  vector<int> histogram(nmax + 1, 0);

  tbb::parallel_for(
      range(0, nmax + 1),
      [&](range r) {
        for (size_t i = r.begin(); i != r.end(); ++i) {
          histogram[i] = reduce2d_with_filter(nrows, ncols, matrix, op_sum,
            [=](int acc, int value) {
              return acc + (value == i);
            });
        }
      });

  vector<int> prefixsum(nmax + 1);
  ScanSum scan_sum(&prefixsum, histogram);
  tbb::parallel_scan(
      range(0, nmax + 1),
      scan_sum,
      tbb::auto_partitioner());

  int count = (nrows * ncols * percent) / 100;
  count = nrows * ncols - count;  // because we scan from left to right
  int threshold = lower_bound(prefixsum.begin(), prefixsum.end(), count) -
     prefixsum.begin();

  tbb::parallel_for(
      range(0, nrows),
      [&](range r) {
        for (size_t i = r.begin(); i != r.end(); ++i) {
          for (int j = 0; j < ncols; j++) {
            (*mask)[i][j] = matrix[i][j] >= threshold;
          }
        }
      });
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
