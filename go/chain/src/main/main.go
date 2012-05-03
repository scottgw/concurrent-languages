/*
 * chain: chain all problems
 *
 * input:
 *   nelts: the number of elements
 *   randmat_seed: random number generator seed
 *   thresh_percent: percentage of cells to retain
 *   winnow_nelts: the number of points to select
 *
 * output:
 *   result: a real vector, whose values are the result of the final product
 *
 */
package main

import (
    "fmt"
    "all"
)

func main() {
  var nelts, randmat_seed int;
  fmt.Scanf("%d%d", &nelts, &randmat_seed);
  fmt.Printf("nelts: %d\n", nelts);
  all.Randmat(nelts, nelts, randmat_seed);
}
