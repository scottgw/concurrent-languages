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
#include <cstring>

#include <algorithm>
#include <iostream>
#include <vector>

#include "tbb/blocked_range.h"
#include "tbb/parallel_for.h"
#include "tbb/task_scheduler_init.h"

using namespace std;
using namespace tbb;

static int is_bench = 0;
int n_threads = task_scheduler_init::default_num_threads();

static pair<int, int> points[10000];
static double matrix[10000][10000];
static double vec[10000];

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
            matrix[i][j] = ::distance(points[i], points[j]);
            nmax = max(nmax, matrix[i][j]);
          }
        }
        matrix[i][i] = nelts * nmax;
        vec[i] = ::distance(make_pair(0, 0), points[i]);
      }
    });
}

void read_vector_of_points(int nelts) {
  for (int i = 0; i < nelts; i++) {
    cin >> points[i].first >> points[i].second;
  }
}

int main(int argc, char** argv) {
  int nelts;

  for (int i = 1; i < argc; i++) {
    if (!strcmp(argv[i], "--is_bench")) {
      is_bench = 1;
    } else if (!strcmp(argv[i], "--threads")) {
      sscanf(argv[i + 1], "%d", &n_threads);
      i++;
    }
  }

  task_scheduler_init init(n_threads);

  scanf("%d", &nelts);

  if (!is_bench) {
    read_vector_of_points(nelts);
  }

  outer(nelts);

  if (!is_bench) {
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
  }

  return 0;
}
