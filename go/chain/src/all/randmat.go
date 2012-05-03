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

func Randmat(nrows, ncols, s int) [][]int {
  matrix := make([][]int, nrows);
  for i := 0; i < nrows; i++ {
    matrix[i] = make([]int, ncols);
  }
  rand.Seed(int64(s));
  split(0, nrows, func(i int) {
      for j := 0; j < ncols; j++ {
        matrix[i][j] = rand.Int() % 1000;
      }
    });

  return matrix;
}

