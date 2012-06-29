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
  "flag"
)

var is_bench = flag.Bool("is_bench", false, "")
var matrix [20000][20000]int;

func randmat(nrows, ncols, s int) {
  LCG_A := 1664525;
  LCG_C := 1013904223;
  for i := 0; i < nrows; i++ {
    seed := s + i;
    for j := 0; j < ncols; j++ {
      seed = LCG_A * seed + LCG_C;
      matrix[i][j] = ((seed % 100) + 100) % 100;
    }
  }
}

func main() {
  var nrows, ncols, s int;

  flag.Parse();

  fmt.Scanf("%d%d%d", &nrows, &ncols, &s);

  randmat(nrows, ncols, s);

  if (!*is_bench) {
    for i := 0; i < nrows; i++ {
      for j := 0; j < ncols; j++ {
        fmt.Printf("%d ", matrix[i][j]);
      }
      fmt.Printf("\n");
    }
    fmt.Printf("\n");
  }
}
