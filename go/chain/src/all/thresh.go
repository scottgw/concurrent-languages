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
package all

func sum(a, b int) int {
  return a + b;
}

func reduce2d_worker(n int, array []int, op func(acc, x int) int,
    start_value int, result chan int) {
  res := start_value;
  for i := 0; i < n; i++ {
    res = op(res, array[i]);
  }
  result <- res;
}

func reduce2d(nrows, ncols int, matrix [][]int,
    aggregator func(acc, x int) int, aggregator_start_value int,
    op func(acc, x int) int, op_start_value int) int {
  response := make(chan int);
  // parallel reduce on rows
  for i := 0; i < nrows; i++ {
    go reduce2d_worker(ncols, matrix[i], op, op_start_value, response);
  }

  results := make([]int, nrows);
  for i := 0; i < nrows; i++ {
    results[i] = <-response
  }

  go reduce2d_worker(nrows, results, aggregator, aggregator_start_value,
      response);

  return <-response;
}

func get_filter(value int) (func(acc, x int) int) {
  return func(acc, x int) int {
    if x == value {
      return acc + 1;
    }
    return acc;
  };
}

func get_fill_histogram(nrows int, ncols int, matrix [][]int,
    histogram []int) (func (index int)) {
  return func(index int) {
    filter := get_filter(index);
    histogram[index] = reduce2d(nrows, ncols, matrix, sum, 0, filter, 0);
  }
}

func get_fill_mask(ncols int, matrix [][]int, mask [][]int,
    threshold int) (func(index int)) {
  return func(index int) {
    for j := 0; j < ncols; j++ {
      if matrix[index][j] >= threshold {
        mask[index][j] = 1;
      }
    }
  }
}

func max_function(a, b int) int {
  if a > b {
    return a;
  }
  return b;
}

func Thresh(nrows, ncols int, matrix [][]int, percent int) [][]int {
  mask := make([][]int, nrows);
  for i := 0; i < nrows; i++ {
    mask[i] = make([]int, ncols);
  }

  nmax := reduce2d(nrows, ncols, matrix, max_function, 0, max_function, 0);
  histogram := make([]int, nmax + 1);
  // parallel for on [0, nmax + 1)
  split(0, nmax + 1, get_fill_histogram(nrows, ncols, matrix, histogram));

  count := (nrows * ncols * percent) / 100;
  prefixsum := 0;
  threshold := nmax;

  for i := nmax; i >= 0 && prefixsum <= count; i-- {
    prefixsum += histogram[i];
    threshold = i;
  }

  // parallel for on rows
  split(0, nrows, get_fill_mask(ncols, matrix, mask, threshold));

  return mask;
}

