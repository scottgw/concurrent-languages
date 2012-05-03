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
