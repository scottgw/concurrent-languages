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
proc product(nelts: int,
    matrix: [1..nelts, 1..nelts] real,
    vector: [1..nelts] real,
    result: [1..nelts] real) {
  for i in 1..nelts do {
    var sum: real = 0;
    for j in 1..nelts do {
      sum += matrix[i, j] * vector[j];
    }
    result[i] = sum;
  }
}

}
