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

proc main() {
  var nelts: int;
  read(nelts);

  var matrix: [1..nelts, 1..nelts] real;
  var vector, result: [1..nelts] real;

  for i in 1..nelts do {
    for j in 1..nelts do {
      read(matrix[i, j]);
    }
  }

  for i in 1..nelts do {
    read(vector[i]);
  }

  product(nelts, matrix, vector, result);

  writeln(nelts);
  for i in 1..nelts do {
    write(result[i] + " ");
  }
  writeln();
}
