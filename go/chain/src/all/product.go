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

func Product(nelts int, matrix [][]Double, vector []Double) []Double {
  result := make([]Double, nelts);
  split(0, nelts, func(i int) {
    var sum Double = 0;
    for j := 0; j < nelts; j++ {
      sum += matrix[i][j] * vector[j];
    }
    result[i] = sum;
  });
  return result;
}

func read_double() Double {
  var value Double;
  for true {
    var read, _ = fmt.Scanf("%g", &value);
    if read == 1 {
      break;
    }
  }
  return value;
}

func read_matrix(nelts int) [][]Double {
  var matrix [][]Double;
  matrix = make([][]Double, nelts);
  for i := 0; i < nelts; i++ {
    matrix[i] = make([]Double, nelts);
    for j := 0; j < nelts; j++ {
      matrix[i][j] = read_double();
    }
  }
  return matrix;
}

func read_vector(nelts int) []Double {
  var vector []Double;
  vector = make([]Double, nelts);
  for i := 0; i < nelts; i++ {
    vector[i] = read_double();
  }
  return vector;
}

