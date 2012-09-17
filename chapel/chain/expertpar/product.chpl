/* product: matrix-vector product
 * 
 * input:
 *   nelts: the number of elements
 *   matrix: a real matrix
 *   vector: a real vector
 *
 * output:
 *    result: a real vector, whose values are the result of the product
 */

module Product {

use Config;

proc product(matrix: [distSpace] real,
             vector: [vectorSpace] real,
             nelts: int):
            [vectorSpace] real 
{
  var result: [vectorSpace] real;

  forall i in 1..nelts do {
    var sum: real = 0;
    for j in 1..nelts do {
      sum += matrix[i, j] * vector[j];
    }
    result[i] = sum;
  }

  return result;
}

}
