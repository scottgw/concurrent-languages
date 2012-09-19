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
config const nrows = read (int),
             ncols = read (int),
             s     = read (int);
var matrix: [1..20000, 1..20000]int;

proc randmat() {
  const LCG_A: int = 1664525,
        LCG_C: int = 1013904223;
  const cols = 1 .. ncols;
  forall i in 1..nrows do {
    var seed = s + i;
    for j in cols do {
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
