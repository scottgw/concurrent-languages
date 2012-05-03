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
package all

import (
  "fmt"
)

func Product(nelts int, matrix [][]double, vector []double,
    result []double) {
  split(0, nelts, func(i int) {
    var sum double = 0;
    for j := 0; j < nelts; j++ {
      sum += matrix[i][j] * vector[j];
    }
    result[i] = sum;
  });
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

