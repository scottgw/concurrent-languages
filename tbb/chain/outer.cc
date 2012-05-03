/* outer: outer product
 *
 * input:
 *   points: a vector of (x, y) points
 *   nelts: the number of points
 *
 * output:
 *   matrix: a real matrix, whose values are filled with inter-point
 *     distances
 *   vec: a real vector, whose values are filled with origin-to-point
 *     distances
 */
#include <cmath>
#include <algorithm>
#include <vector>

#include "tbb/blocked_range.h"
#include "tbb/parallel_for.h"

using namespace std;
using namespace tbb;

typedef blocked_range<size_t> range;

double sqr(double x) {
  return x * x;
}

double distance(const pair<int, int>& x, const pair<int, int>& y) {
  return sqrt(sqr(x.first - y.first) + sqr(x.second - y.second));
}

void outer(int nelts, const vector<pair<int, int> > & points,
    vector<vector<double> >* matrix, vector<double>* vec) {
  parallel_for(
    range(0, nelts),
    [&](range r) {
      for (size_t i = r.begin(); i != r.end(); ++i) {
        double nmax = -1;
        for (int j = 0; j < nelts; j++) {
          if (i != j) {
            (*matrix)[i][j] = ::distance(points[i], points[j]);
            nmax = max(nmax, (*matrix)[i][j]);
          }
        }
        (*matrix)[i][i] = nelts * nmax;
        (*vec)[i] = ::distance(make_pair(0, 0), points[i]);
      }
    });
}

