/*
 * outer: outer product
 *
 * input:
 *   vector: a vector of (x, y) points
 *   nelts: the number of points
 *
 * output:
 *   matrix: a real matrix, whose values are filled with inter-point
 *     distances
 *   vector: a real vector, whose values are filled with origin-to-point
 *     distances
 */
package all

import (
  "math"
)

type Point struct {
  value, i, j int;
}

type Double float64;

type Points []Point;

func max(a, b Double) Double {
  if a > b {
    return a;
  }
  return b;
}

func sqr(x Double) Double {
  return x * x;
}

func distance(a, b Point) Double {
  return Double(math.Sqrt(float64(sqr(Double(a.i - b.i)) +
        sqr(Double(a.j - b.j)))));
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

func Outer(nelts int, points Points, matrix [][]Double, vector []Double) {
  split(0, nelts, func(i int) {
    var nmax Double = -1;
    for j := 0; j < nelts; j++ {
      if (i != j) {
        matrix[i][j] = distance(points[i], points[j]);
        nmax = max(nmax, matrix[i][j]);
      }
    }
    matrix[i][i] = Double(nelts) * nmax;
    vector[i] = distance(Point{i: 0, j: 0}, points[i]);
  });
}

