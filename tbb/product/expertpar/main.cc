/* product: matrix-vector product
 *
 * input:
 *   matrix: a real matrix
 *   vec: a real vector
 *   nelts: the number of elements
 *
 * output:
 *   result: a real vector, whose values are the result of the product
 */
#include <cstdio>
#include <cstring>

#include <iostream>
#include <vector>

#include "tbb/tbb.h"

using namespace std;
using namespace tbb;

static int is_bench = 0;
int n_threads = task_scheduler_init::default_num_threads();

static double matrix[10000][10000];
static double vec[10000];
static double result[10000];

typedef blocked_range2d<size_t, size_t> range;

void product(int nelts) {
  parallel_for(
    range(0, nelts, 0, nelts),
    [&, nelts](range r) {
      auto r_end = r.rows().end();
      for (size_t i = r.rows().begin(); i != r_end; ++i) {
        auto c_end = r.cols().end();
        for (size_t j = r.cols().begin(); j != c_end; ++j) {
          result [i] += matrix[i][j] * vec[j];
        }
      }
  });
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

  cin >> nelts;

  if (!is_bench) {
    for (int i = 0; i < nelts; i++) {
      for (int j = 0; j < nelts; j++) {
        cin >> matrix[i][j];
      }
    }

    for (int i = 0; i < nelts; i++) {
      cin >> vec[i];
    }
  }

  product(nelts);

  if (!is_bench) {
    printf("%d\n", nelts, nelts);
    for (int i = 0; i < nelts; i++) {
      printf("%g ", result[i]);
    }
    printf("\n");
  }

  return 0;
}
