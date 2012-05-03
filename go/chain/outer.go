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
package main

import (
  "fmt"
  "math"
)

type Point struct {
  i, j int;
}

type double float64;

type Points []Point;

func max(a, b double) double {
  if a > b {
    return a;
  }
  return b;
}

func sqr(x double) double {
  return x * x;
}

func distance(a, b Point) double {
  return double(math.Sqrt(float64(sqr(double(a.i - b.i)) +
        sqr(double(a.j - b.j)))));
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

func outer(nelts int, points Points, matrix [][]double, vector []double) {
  split(0, nelts, func(i int) {
    var nmax double = -1;
    for j := 0; j < nelts; j++ {
      if (i != j) {
        matrix[i][j] = distance(points[i], points[j]);
        nmax = max(nmax, matrix[i][j]);
      }
    }
    matrix[i][i] = double(nelts) * nmax;
    vector[i] = distance(Point{0, 0}, points[i]);
  });
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

func create_matrix(nelts int) [][]double {
  var matrix [][]double;
  matrix = make([][]double, nelts);
  for i := 0; i < nelts; i++ {
    matrix[i] = make([]double, nelts);
  }
  return matrix;
}

func read_vector_of_points(nelts int) Points {
  var vector Points;
  vector = make(Points, nelts);
  for i := 0; i < nelts; i++ {
    a := read_integer();
    b := read_integer();
    vector[i] = Point{a, b};
  }
  return vector;
}

func main() {
  var nelts int;
  var points Points;
  var matrix [][]double;
  var vector []double;

  nelts = read_integer();
  points = read_vector_of_points(nelts);
  matrix = create_matrix(nelts);
  vector = make([]double, nelts);

  outer(nelts, points, matrix, vector);

  fmt.Printf("%d %d\n", nelts, nelts);
  for i := 0; i < nelts; i++ {
    for j := 0; j < nelts; j++ {
      fmt.Printf("%g ", matrix[i][j]);
    }
    fmt.Printf("\n");
  }
  fmt.Printf("\n");

  fmt.Printf("%d\n", nelts);
  for i := 0; i < nelts; i++ {
    fmt.Printf("%g ", vector[i]);
  }
  fmt.Printf("\n");
}
