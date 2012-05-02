/*
 * product: matrix-vector product
 *
 * input:
 *   nelts: the number of elements
 *   matrix: the real matrix
 *   vector: the real vector
 *
 * output:
 *   result: a real vector, whose values are the result of the product
 */
package main

import (
  "fmt"
)

type double float64;

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

func product(nelts int, matrix [][]double, vector []double,
    result []double) {
  split(0, nelts, func(i int) {
    var sum double = 0;
    for j := 0; j < nelts; j++ {
      sum += matrix[i][j] * vector[j];
    }
    result[i] = sum;
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

func read_double() double {
  var value double;
  for true {
    var read, _ = fmt.Scanf("%g", &value);
    if read == 1 {
      break;
    }
  }
  return value;
}

func read_matrix(nelts int) [][]double {
  var matrix [][]double;
  matrix = make([][]double, nelts);
  for i := 0; i < nelts; i++ {
    matrix[i] = make([]double, nelts);
    for j := 0; j < nelts; j++ {
      matrix[i][j] = read_double();
    }
  }
  return matrix;
}

func read_vector(nelts int) []double {
  var vector []double;
  vector = make([]double, nelts);
  for i := 0; i < nelts; i++ {
    vector[i] = read_double();
  }
  return vector;
}

func main() {
  var nelts int;
  var matrix [][]double;
  var vector, result []double;

  nelts = read_integer();
  matrix = read_matrix(nelts);
  vector = read_vector(nelts);
  result = make([]double, nelts);

  product(nelts, matrix, vector, result);

  fmt.Printf("%d\n", nelts);
  for i := 0; i < nelts; i++ {
    fmt.Printf("%g ", result[i]);
  }
  fmt.Printf("\n");
}
