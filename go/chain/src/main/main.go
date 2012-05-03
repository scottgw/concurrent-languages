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

func create_matrix_double(nelts int) [][]all.Double {
  var matrix [][]all.Double;
  matrix = make([][]all.Double, nelts);
  for i := 0; i < nelts; i++ {
    matrix[i] = make([]all.Double, nelts);
  }
  return matrix;
}

func read_integer() int {
  var value int;
  for true {
    var read, _ = fmt.Scanf("%d", &value);
    if read == 1 {
      break;
    }
  }
  return value;
}

func main() {
  nelts := read_integer();
  randmat_seed := read_integer();
  thresh_percent := read_integer();
  winnow_nelts := read_integer();

  outer_matrix := create_matrix_double(winnow_nelts);
  outer_vector := make([]all.Double, winnow_nelts);

  randmat_matrix := all.Randmat(nelts, nelts, randmat_seed);
  thresh_mask := all.Thresh(nelts, nelts, randmat_matrix, thresh_percent);
  winnow_points := all.Winnow(nelts, nelts, randmat_matrix, thresh_mask,
      winnow_nelts);
  all.Outer(winnow_nelts, winnow_points, outer_matrix, outer_vector);
  product_result := all.Product(winnow_nelts, outer_matrix, outer_vector);

  fmt.Printf("%d\n", winnow_nelts);
  for i := 0; i < winnow_nelts; i++ {
    fmt.Printf("%g ", product_result[i]);
  }
  fmt.Printf("\n");
}
