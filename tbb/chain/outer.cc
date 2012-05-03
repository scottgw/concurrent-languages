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
#include <cassert>
#include <cmath>
#include <cstdio>
#include <cstdlib> 
#include <algorithm>
#include <iostream>
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

void read_vector_of_points(int nelts, vector<pair<int, int> >* vec) {
  for (int i = 0; i < nelts; i++) {
    cin >> (*vec)[i].first >> (*vec)[i].second;
  }
}

int main(int argc, char** argv) {
  int nelts;
  scanf("%d", &nelts);

  vector<pair<int, int> > points(nelts);
  read_vector_of_points(nelts, &points);

  vector<vector<double> > matrix(nelts, vector<double>(nelts));
  vector<double> vec(nelts);

  outer(nelts, points, &matrix, &vec);

  printf("%d %d\n", nelts, nelts);
  for (int i = 0; i < nelts; i++) {
    for (int j = 0; j < nelts; j++) {
      printf("%g ", matrix[i][j]);
    }
    printf("\n");
  }
  printf("\n");

  printf("%d\n", nelts);
  for (int i = 0; i < nelts; i++) {
    printf("%g ", vec[i]);
  }
  printf("\n");

  return 0;
}
