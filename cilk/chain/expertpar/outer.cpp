/*
 * outer: outer product
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

#include <cilk/cilk.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <algorithm>

using namespace std;

double outer_matrix[10000][10000];
double outer_vector[10000];

typedef std::pair <int, int> point; 

extern point winnow_points[10000];

double sqr(double x) {
  return x * x;
}

double distance(point a, point b) {
  return sqrt(sqr(a.first - b.first) + sqr(a.second - b.second));
}

void outer (int nelts) {
  cilk_for (int i = 0; i < nelts; ++i) {
    double nmax = 0;
    for (int j = 0; j < nelts; ++j) {
      if (i != j) {
        outer_matrix [i][j] = ::distance (winnow_points [i], winnow_points [j]);
        nmax = max (nmax, outer_matrix [i][j]);
      }      
    }
    outer_matrix [i][i] = nmax * nelts;
    outer_vector [i] = ::distance (make_pair (0,0), winnow_points [i]);
  }
}
