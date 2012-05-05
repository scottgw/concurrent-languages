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

func randvec(vector []int, n int, done chan bool) {
  for i := 0; i < n; i++ {
    vector[i] = rand.Int();
  }
  done <- true;
}

func randmat(nrows, ncols, s int) [][]int {
  var matrix [][]int;
  matrix = make([][]int, nrows);
  for i := 0; i < nrows; i++ {
    matrix[i] = make([]int, ncols);
  }
  rand.Seed(int64(s));
  done := make(chan bool);
  // parallel for on rows
  for i := 0; i < nrows; i++ {
    go randvec(matrix[i], ncols, done);
  }
  for i := 0; i < nrows; i++ {
    <-done;
  }
  return matrix;
}

func main() {
  var nrows, ncols, s int;
  var matrix [][]int;

  fmt.Scanf("%d%d%d", &nrows, &ncols, &s);

  matrix = randmat(nrows, ncols, s);

  for i := 0; i < nrows; i++ {
    for j := 0; j < ncols; j++ {
      fmt.Printf("%d ", matrix[i][j]);
    }
    fmt.Printf("\n");
  }
  fmt.Printf("\n");
}
