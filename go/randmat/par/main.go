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

func randvec(vector []int, n int, r *rand.Rand, done chan bool) {
  for i := 0; i < n; i++ {
    vector[i] = r.Int();
  }
  done <- true;
}

func randmat(nrows, ncols, s int, matrix [][]int) {
  //rand.Seed(int64(s));
  done := make(chan bool);
  // parallel for on rows
  for i := 0; i < nrows; i++ {
    r := rand.New(rand.NewSource(int64(s + i)));
    go randvec(matrix[i], ncols, r, done);
  }
  for i := 0; i < nrows; i++ {
    <-done;
  }
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
