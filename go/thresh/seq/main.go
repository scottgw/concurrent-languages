/*
 * thresh: histogram thresholding
 *
 * input:
 *   matrix: the integer matrix to be thresholded
 *   nrows, ncols: the number of rows and columns
 *   percent: the percentage of cells to retain
 *
 * output:
 *   mask: a boolean matrix filled with true for cells that are kept
 *
 */
package main

import (
  "fmt"
)

func max(a, b int) int {
  if a > b {
    return a;
  }
  return b;
}

func thresh(nrows, ncols int, matrix [][]int, percent int) [][]int {
  var mask[][]int;
  mask = make([][]int, nrows);
  for i := 0; i < nrows; i++ {
    mask[i] = make([]int, ncols);
  }

  var nmax int = 0;
  for i := 0; i < nrows; i++ {
    for j := 0; j < ncols; j++ {
      nmax = max(nmax, matrix[i][j]);
    }
  }

  var histogram []int;
  histogram = make([]int, nmax + 1);

  for i := 0; i < nrows; i++ {
    for j := 0; j < ncols; j++ {
      histogram[matrix[i][j]]++;
    }
  }

  var count int = (nrows * ncols * percent) / 100;
  var prefixsum int = 0;
  var threshold int = nmax;

  for i := nmax; i >= 0 && prefixsum <= count; i-- {
    prefixsum += histogram[i];
    threshold = i;
  }


  for i := 0; i < nrows; i++ {
    for j := 0; j < ncols; j++ {
      if matrix[i][j] >= threshold {
        mask[i][j] = 1;
      }
    }
  }

  return mask;
}

func main() {
  var nrows, ncols, percent int;
  var matrix, mask [][]int;

  fmt.Scanf("%d%d", &nrows, &ncols);

  matrix = make([][]int, nrows);
  for i := 0; i < nrows; i++ {
    matrix[i] = make([]int, ncols);
  }

  for i := 0; i < nrows; i++ {
    for j := 0; j < ncols; j++ {
      fmt.Scanf("%d", &matrix[i][j]);
    }
  }

  fmt.Scanf("%d", &percent);

  mask = thresh(nrows, ncols, matrix, percent);

  for i := 0; i < nrows; i++ {
    for j := 0; j < ncols; j++ {
      fmt.Printf("%d ", mask[i][j]);
    }
    fmt.Printf("\n");
  }
  fmt.Printf("\n");
}
