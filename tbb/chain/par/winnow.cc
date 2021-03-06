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

#include "tbb/blocked_range.h"
#include "tbb/parallel_for.h"
#include "tbb/parallel_scan.h"
#include "tbb/task_scheduler_init.h"
#include "tbb/parallel_reduce.h"

using namespace std;
using namespace tbb;

extern int is_bench;
extern unsigned char randmat_matrix[20000][20000];
extern unsigned char thresh_mask[20000][20000];
static int count_per_line[20001];
static int total_count[20001];
pair<int, int> winnow_points[20000];
static pair<int, pair<int, int> > values[20000];

typedef blocked_range<size_t> range;

class ScanSum {
  int sum;

public:
  ScanSum(): sum(0) {}
  template<typename Tag>
  void operator()(range r, Tag) {
    int res = sum;
    for (size_t i = r.begin(); i != r.end(); ++i) {
      res += count_per_line[i];
      if (Tag::is_final_scan()) {
        total_count[i] = res;
      }
    }
    sum = res;
  }
  ScanSum(ScanSum& other, tbb::split) : sum(0) {}
  void reverse_join(ScanSum& other) { sum += other.sum; }
  void assign(ScanSum& other) { sum = other.sum; }
};

void winnow(int nrows, int ncols, int nelts) {
  int count = 0;

  count = parallel_reduce(
    range(0, nrows), 0,
    [=](range r, int result)->int {
      for (size_t i = r.begin(); i != r.end(); i++) {
        int cur = 0;
        for (int j = 0; j < ncols; j++) {
          if (is_bench) {
            thresh_mask[i][j] = ((i * j) % (ncols + 1)) == 1;
          }
          cur += thresh_mask[i][j];
        }
        result += count_per_line[i + 1] = cur;
      }
      return result;
    },
    [](int x, int y)->int {
      return x + y;
    });

  ScanSum scan_sum;
  tbb::parallel_scan(
      range(0, nrows + 1),
      scan_sum,
      tbb::auto_partitioner());

  tbb::parallel_for(
      range(0, nrows),
      [=](range r) {
        for (size_t i = r.begin(); i != r.end(); i++) {
          int count = total_count[i];
          for (int j = 0; j < ncols; j++) {
            if (thresh_mask[i][j]) {
              values[count] = (make_pair(randmat_matrix[i][j],
                  make_pair(i, j)));
              count++;
            }
          }
        }
      });

  sort(values, values + count);

  size_t n = count;
  size_t chunk = n / nelts;

  for (int i = 0; i < nelts; i++) {
    int index = i * chunk;
    winnow_points[i] = values[index].second;
  }
}
