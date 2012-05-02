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
  "sort"
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

func winnow(nrows, ncols int, matrix, mask [][]int, nelts int) []Point {
  var n = 0;

  for i := 0; i < nrows; i++ {
    for j := 0; j < ncols; j++ {
      if mask[i][j] == 1 {
        n++;
      }
    }
  }

  var points, values Points;
  points = make(Points, n);
  values = make(Points, n);

  var count = 0;
  for i := 0; i < nrows; i++ {
    for j := 0; j < ncols; j++ {
      if mask[i][j] == 1 {
        values[count] = Point{matrix[i][j], i, j};
        count++;
      }
    }
  }
  
  sort.Sort(values);

  var total = len(values);
  var chunk int = total / nelts;

  for i := 0; i < nelts; i++ {
    var index = i * chunk;
    points[i] = values[index];
  }

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
