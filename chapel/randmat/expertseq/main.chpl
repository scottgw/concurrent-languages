/* randmat: random number generation
 * 
 * input:
 *   nrows, ncols: the number of rows and columns
 *   s: the seed
 *
 * output:
 *   matrix: an nrows x ncols integer matrix
 */

use Random;

config const is_bench = false;
config const nrows = read(int (32)),
             ncols = read(int (32)),
             s     = read(int (32));
var matrix: [1..20000, 1..20000] int (32);

proc randmat() {
  const LCG_A: uint(32) = 1664525,
        LCG_C: uint(32) = 1013904223;
  for i in 1..nrows do {
    var seed: int(32) = s + i - 1;
    for j in 1..ncols do {
      seed = LCG_A * seed + LCG_C;
      matrix[i, j] = abs(seed) % 100;
    }
  }
}

proc main() {

  randmat();

  if (!is_bench) {
    writeln(nrows, " ", ncols);

    for i in 1..nrows do {
      for j in 1..ncols do {
        write(matrix[i, j], " ");
      }
      writeln();
    }
    writeln();
  }
}
