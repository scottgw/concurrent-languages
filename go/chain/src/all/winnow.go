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
package all

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

func get_count_func(ncols int, matrix, mask [][]int, result chan Point) (
    func(index int)) {
  return func(index int) {
    for j := 0; j < ncols; j++ {
      if mask[index][j] == 1 {
        result <- Point{value: matrix[index][j], i: index, j: j};
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

func Winnow(nrows, ncols int, matrix, mask [][]int, nelts int) []Point {
  n := reduce2d(nrows, ncols, mask, sum, 0, sum, 0);

  points := make(Points, n);
  values := make(Points, n);

  result := make(chan Point, n);
  split(0, nrows, get_count_func(ncols, matrix, mask, result));

  for i := 0; i < n; i++ {
    values[i] = <-result;
  }

  sort(n, values);

  total := len(values);
  var chunk int = total / nelts;

  split(0, nelts, func(i int) {
      index := i * chunk;
      points[i] = values[index];
    });

  return points;
}

