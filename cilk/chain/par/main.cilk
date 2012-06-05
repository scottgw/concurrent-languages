/*
 * chain: chain all problems
 *
 * input:
 *   nelts: the number of elements
 *   randmat_seed: random number generator of cells to retain
 *   thresh_percent: percentage of cells to retain
 *   winnow_nelts: the number of points to select
 *
 * output:
 *   result: a real vector, whose values are the result of the final product
 */
#include <cilk-lib.cilkh>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "common.cilkh"

int is_bench = 0;

cilk void randmat(int, int, int);
cilk void thresh(int, int, int);
cilk void winnow(int, int, int);
cilk void outer(int);
cilk void product(int);

extern double product_result[10000];

cilk int main(int argc, char** argv) {
  int nelts, randmat_seed, thresh_percent, winnow_nelts, i;

  if (argc == 2) {
    if (!strcmp(argv[argc - 1], "--is_bench")) {
      is_bench = 1;
    }
  }

  scanf("%d%d%d%d", &nelts, &randmat_seed, &thresh_percent, &winnow_nelts);

  spawn randmat(nelts, nelts, randmat_seed); sync;
  spawn thresh(nelts, nelts, thresh_percent); sync;
  spawn winnow(nelts, nelts, winnow_nelts); sync;
  spawn outer(winnow_nelts); sync;
  spawn product(winnow_nelts); sync;

  if (!is_bench) {
    printf("%d\n", winnow_nelts);
    for (i = 0; i < winnow_nelts; i++) {
      printf("%g ", product_result[i]);
    }
    printf("\n");
  }

  return 0;
}

