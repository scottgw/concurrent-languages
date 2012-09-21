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

config const is_bench = false;
config const nelts = read(int);

const Space2 = [1..nelts, 1..nelts];

var matrix: [Space2] real;
var vector: [1..10000] real;
var result: [1..10000] real;

proc product(nelts: int) {
  for (i,j) in Space2 do {
    result [i] += matrix [i, j] * vector [j];
  }
}

proc main() {
  if (!is_bench) {
    for i in 1..nelts do {
      for j in 1..nelts do {
        read(matrix[i, j]);
      }
    }

    for i in 1..nelts do {
      read(vector[i]);
    }
  }

  product(nelts);

  if (!is_bench) {
    writeln(nelts);
    for i in 1..nelts do {
      write(result[i] + " ");
    }
    writeln();
  }
}
