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
package all

import (
  "math/rand"
)

func randvec(vector []int, n int, done chan bool) {
  for i := 0; i < n; i++ {
    vector[i] = rand.Int() % 1000;
  }
  done <- true;
}

func Randmat(nrows, ncols, s int) [][]int {
  var matrix [][]int;
  matrix = make([][]int, nrows);
  for i := 0; i < nrows; i++ {
    matrix[i] = make([]int, ncols);
  }
  rand.Seed(int64(s));
  done := make(chan bool);
  for i := 0; i < nrows; i++ {
    go randvec(matrix[i], ncols, done);
  }
  for i := 0; i < nrows; i++ {
    <-done;
  }
  return matrix;
}

