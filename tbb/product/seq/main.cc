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

using namespace std;

static int is_bench = 0;

static double matrix[10000][10000];
static double vec[10000];
static double result[10000];

void product(int nelts) {
  for (int i = 0; i < nelts; i++) {
    double sum = 0;
    for (int j = 0; j < nelts; j++) {
      sum += matrix[i][j] * vec[j];
    }
    result[i] = sum;
  }
}

int main(int argc, char** argv) {
  int nelts;

  for (int i = 1; i < argc; i++) {
    if (!strcmp(argv[i], "--is_bench")) {
      is_bench = 1;
    }
  }

  scanf("%d", &nelts);

  if (!is_bench) {
    /*
    for (int i = 0; i < nelts; i++) {
      for (int j = 0; j < nelts; j++) {
        cin >> matrix[i][j];
      }
    }

    for (int i = 0; i < nelts; i++) {
      cin >> vec[i];
    }
    //*/
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
