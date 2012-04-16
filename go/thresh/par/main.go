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

func worker(n int, array []int, op func(a, b int) int, result chan int) {
  var res int;
  if n == 1 {
    res = array[0];
  } else {
    res = op(array[0], array[1]);
  }
  for i := 2; i < n; i++ {
    res = op(res, array[i]);
  }
  result <- res;
}

func reduce2d(nrows, ncols int, matrix [][]int, op func(a, b int) int) int {
  response := make(chan int);
  for i := 0; i < nrows; i++ {
    go worker(ncols, matrix[i], op, response);
  }

  results := make([]int, nrows);
  for i := 0; i < nrows; i++ {
    results[i] = <-response
  }

  go worker(nrows, results, op, response);

  return <-response;
}

func thresh(nrows, ncols int, matrix [][]int, percent int) [][]int {
  mask := make([][]int, nrows);
  for i := 0; i < nrows; i++ {
    mask[i] = make([]int, ncols);
  }

  nmax := reduce2d(nrows, ncols, matrix, max);
  histogram := make([]int, nmax + 1);

  for i := 0; i < nrows; i++ {
    for j := 0; j < ncols; j++ {
      histogram[matrix[i][j]]++;
    }
  }

  count := (nrows * ncols * percent) / 100;
  prefixsum := 0;
  threshold := nmax;

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

  fmt.Scanf("%d%d", &nrows, &ncols);

  matrix := make([][]int, nrows);
  for i := 0; i < nrows; i++ {
    matrix[i] = make([]int, ncols);
  }

  for i := 0; i < nrows; i++ {
    for j := 0; j < ncols; j++ {
      fmt.Scanf("%d", &matrix[i][j]);
    }
  }

  fmt.Scanf("%d", &percent);

  mask := thresh(nrows, ncols, matrix, percent);

  for i := 0; i < nrows; i++ {
    for j := 0; j < ncols; j++ {
      fmt.Printf("%d ", mask[i][j]);
    }
    fmt.Printf("\n");
  }
  fmt.Printf("\n");
}
