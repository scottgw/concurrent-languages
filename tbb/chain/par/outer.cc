/* outer: outer product
 *
 * input:
 *   winnow_points: a vector of (x, y) points
 *   nelts: the number of points
 *
 * output:
 *   outer_matrix: a real matrix, whose values are filled with inter-point
 *     distances
 *   outer_vector: a real vector, whose values are filled with origin-to-point
 *     distances
 */
#include <cassert>
#include <cmath>
#include <cstdio>
#include <cstdlib> 
#include <cstring>

#include <algorithm>
#include <iostream>
#include <vector>

#include "tbb/blocked_range.h"
#include "tbb/parallel_for.h"
#include "tbb/task_scheduler_init.h"

using namespace std;
using namespace tbb;

extern pair<int, int> winnow_points[10000];
double outer_matrix[10000][10000];
double outer_vector[10000];

typedef blocked_range<size_t> range;

double sqr(double x) {
  return x * x;
}

double distance(const pair<int, int>& x, const pair<int, int>& y) {
  return sqrt(sqr(x.first - y.first) + sqr(x.second - y.second));
}

void outer(int nelts) {
  parallel_for(
    range(0, nelts),
    [&](range r) {
      for (size_t i = r.begin(); i != r.end(); ++i) {
        double nmax = -1;
        for (int j = 0; j < nelts; j++) {
          if (i != j) {
            outer_matrix[i][j] = ::distance(winnow_points[i], winnow_points[j]);
            nmax = max(nmax, outer_matrix[i][j]);
          }
        }
        outer_matrix[i][i] = nelts * nmax;
        outer_vector[i] = ::distance(make_pair(0, 0), winnow_points[i]);
      }
    });
}