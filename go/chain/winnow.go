/*
 * winnow: weighted point selection
 *
 * input:
 *   matrix: an integer matrix, whose values are used as masses
 *   mask: a boolean matrix showing which points are eligible for
 *     consideration
 *   nrows, ncols: the number of rows and columns
 *   nelts: the number of points to select
 *
 * output:
 *   points: a vector of (x, y) points
 *
 */
package main

import (
  "fmt"
)

type Point struct {
  value, i, j int;
}

type Points []Point;

func (p Points) Len() int { return len(p) }
func (p Points) Swap(i, j int) { p[i], p[j] = p[j], p[i] }
func (p Points) Less(i, j int) bool {
  if p[i].value < p[j].value {
    return true;
  }
  if p[i].value > p[j].value {
    return false;
  }
  if p[i].i < p[j].i {
    return true;
  }
  if p[i].i > p[j].i {
    return false;
  }
  return p[i].j < p[j].j;
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

func sum(a, b int) int {
  return a + b;
}

func split_worker(index int, op func(index int), done chan bool) {
  op(index);
  done <- true;
}
  
func split(begin, end int, op func(index int)) {
  done := make(chan bool);
  for i := begin; i < end; i++ {
    go split_worker(i, op, done)
  }
  for i := begin; i < end; i++ {
    <-done;
  }
}

func get_count_func(ncols int, matrix, mask [][]int, result chan Point) (
    func(index int)) {
  return func(index int) {
    for j := 0; j < ncols; j++ {
      if mask[index][j] == 1 {
        result <- Point{matrix[index][j], index, j};
      }
    }
  };
}

func sort_impl(begin, end int, values Points) {
  if (begin + 1 >= end) {
    return;
  }
  var pivot_index int = (begin + end) / 2;
  values.Swap(end - 1, pivot_index);
  spot := begin;
  for i := begin; i < end; i++ {
    if values.Less(i, end - 1) {
      values.Swap(i, spot);
      spot++;
    }
  }
  values.Swap(spot, end - 1);
  pivot_index = spot;

  split(0, 2, func(index int) {
      if index == 0 {
        sort_impl(begin, pivot_index, values);
      } else {
        sort_impl(pivot_index + 1, end, values);
      }
    });
}

func sort(n int, values []Point) {
  sort_impl(0, n, values);
}

func winnow(nrows, ncols int, matrix, mask [][]int, nelts int) []Point {
  var n = 0;

  n = reduce2d(nrows, ncols, mask, sum, 0, sum, 0);

  var points, values Points;
  points = make(Points, n);
  values = make(Points, n);

  result := make(chan Point, n);
  split(0, nrows, get_count_func(ncols, matrix, mask, result));

  for i := 0; i < n; i++ {
    values[i] = <-result;
  }

  sort(n, values);

  var total = len(values);
  var chunk int = total / nelts;

  split(0, nelts, func(i int) {
      var index = i * chunk;
      points[i] = values[index];
    });

  return points;
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

func read_matrix(nrows, ncols int) [][]int {
  var matrix [][]int;
  matrix = make([][]int, nrows);
  for i := 0; i < nrows; i++ {
    matrix[i] = make([]int, ncols);
    for j := 0; j < ncols; j++ {
      matrix[i][j] = read_integer();
    }
  }
  return matrix;
}

func main() {
  var nrows, ncols, nelts int;
  var matrix, mask [][]int;

  nrows = read_integer();
  ncols = read_integer();
  matrix = read_matrix(nrows, ncols);
  mask = read_matrix(nrows, ncols);
  nelts = read_integer();

  var points = winnow(nrows, ncols, matrix, mask, nelts);

  fmt.Printf("%d\n", nelts);
  for i := 0; i < nelts; i++ {
    fmt.Printf("%d %d\n", points[i].i, points[i].j);
  }
  fmt.Printf("\n");
}
