/*
 * randmat: random number generation
 *
 * input:
 *   nrows, ncols: the number of rows and columns
 *   s: the seed
 *
 * output:
 *   martix: a nrows x ncols integer matrix
 *
 */
package main

import (
  "fmt"
  "rand"
)

func randmat(nrows, ncols, s int, matrix [][]int) [][]int {
  rand.Seed(int64(s));
  for i := 0; i < nrows; i++ {
    for j := 0; j < ncols; j++ {
      matrix[i][j] = rand.Int();
    }
  }
  return matrix;
}

func main() {
  var nrows, ncols, s int;
  var matrix [][]int;

  fmt.Scanf("%d%d%d", &nrows, &ncols, &s);

  matrix = make([][]int, nrows);
  for i := 0; i < nrows; i++ {
    matrix[i] = make([]int, ncols);
  }

  randmat(nrows, ncols, s, matrix);

  /*
  for i := 0; i < nrows; i++ {
    for j := 0; j < ncols; j++ {
      fmt.Printf("%d ", matrix[i][j]);
    }
    fmt.Printf("\n");
  }
  fmt.Printf("\n");//*/
}
