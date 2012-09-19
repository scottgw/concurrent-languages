/*
 * randmat: random number generation
 *
 * input:
 *   nrows, ncols: the number of rows and columns
 *   s: the seed
 *
 * output:
 *   martix: a nrows x ncols integer matrix
 *
 */
package main

import (
  "fmt"
  "flag"
)

type ByteMatrix struct {
  Rows, Cols int
  x []byte
}

func NewByteMatrix (r, c int) *ByteMatrix {
  return &ByteMatrix {r, c, make ([]byte, r*c)}
}

func (m *ByteMatrix) Row (i int) [] byte {
  return m.X [i*m.Cols : (i+1)*m.Cols]
}

const(
  LCG_A = 1664525
  LCG_C = 1013904223
)

var (
  is_bench = flag.Bool("is_bench", false, "")
  nrows    = flag.Int ("nrows", 10, "rows")
  ncols   = flag.Int ("ncols", 9, "cols")
  seed  =flag.Int ("seed", 8, "seed")
)

func randmat(nrows, ncols, s int) *ByteMatrix {
  matrix := nNewByteMatrix (nrows, ncols)

  for i := 0; i < nrows; i++ {
    seed := s + i;
    row  := matrix.Row (i)
    for j := range row {
      seed = LCG_A * seed + LCG_C;
      row [i] =  (seed % 100) % 100;
    }
  }
}

func main() {
  flag.Parse();

  randmat(nrows, ncols, s);

  if (!*is_bench) {
    for i := 0; i < nrows; i++ {
      for j := 0; j < ncols; j++ {
        fmt.Printf("%d ", matrix[i][j]);
      }
      fmt.Printf("\n");
    }
    fmt.Printf("\n");
  }
}
