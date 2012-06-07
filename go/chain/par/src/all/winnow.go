/*
 * winnow: weighted point selection
 *
 * input:
 *   Randmat_matrix: an integer matrix, whose values are used as masses
 *   Thresh_mask: a boolean matrix showing which points are eligible for
 *     consideration
 *   nrows, ncols: the number of rows and columns
 *   nelts: the number of points to select
 *
 * output:
 *   Winnow_points: a vector of (x, y) points
 *
 */
package all

import (
  "sort"
  "flag"
)

var Is_bench = flag.Bool("is_bench", false, "")

type Point struct {
  value byte;
  i, j int;
}

type Points []Point;

var Winnow_points [10000]Point;
var values Points;

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

func reduce_sum_impl(begin, end, ncols int, done chan int) {
  if (begin + 1 == end) {
    res := 0
    for j := 0; j < ncols; j++ {
      if (*Is_bench) {
        if (((begin * j) % (ncols + 1)) == 1) {
          Thresh_mask[begin][j] = 1;
        }
      }
      res += int(Thresh_mask[begin][j]);
    }
    done <- res;
  } else {
    middle := begin + (end - begin) / 2;
    go reduce_sum_impl(begin, middle, ncols, done);
    reduce_sum_impl(middle, end, ncols, done);
  }
}

func reduce_sum(nrows, ncols int) int {
  done := make(chan int);
  // parallel for on rows
  go reduce_sum_impl(0, nrows, ncols, done);
  res := 0
  for i := 0; i < nrows; i++ {
    res += <-done
  }
  return res
}

func fill_values_impl(begin, end, ncols int, done chan Point) {
  if (begin + 1 == end) {
    for j := 0; j < ncols; j++ {
      if Thresh_mask[begin][j] == 1 {
        done <- Point{Randmat_matrix[begin][j], begin, j};
      }
    }
  } else {
    middle := begin + (end - begin) / 2;
    go fill_values_impl(begin, middle, ncols, done);
    fill_values_impl(middle, end, ncols, done);
  }
}

func fill_values(nrows, ncols, n int) {
  done := make(chan Point);
  // parallel for on rows
  go fill_values_impl(0, nrows, ncols, done);
  for i := 0; i < n; i++ {
    values[i] = <-done;
  }
}

func Winnow(nrows, ncols, nelts int) {
  n := reduce_sum(nrows, ncols);

  values = make(Points, n);
  fill_values(nrows, ncols, n)
  
  sort.Sort(values);

  var total = len(values);
  var chunk int = total / nelts;

  for i := 0; i < nelts; i++ {
    var index = i * chunk;
    Winnow_points[i] = values[index];
  }
}
