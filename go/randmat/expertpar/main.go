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
	"flag"
	"fmt"
)

type ByteMatrix struct {
	Rows, Cols uint32
	array      []byte
}

func NewByteMatrix(r, c uint32) *ByteMatrix {
	return &ByteMatrix{r, c, make([]byte, r*c)}
}

func (m *ByteMatrix) Row(i uint32) []byte {
	return m.array[i*m.Cols : (i+1)*m.Cols]
}

const (
	LCG_A = 1664525
	LCG_C = 1013904223
)

var (
	is_bench = flag.Bool("is_bench", false, "")
	nrows    = flag.Uint("nrows", 10, "rows")
	ncols    = flag.Uint("ncols", 9, "cols")
	seed     = flag.Uint("seed", 8, "seed")
)

func randmat(nrows, ncols, s uint32) *ByteMatrix {
	matrix := NewByteMatrix(nrows, ncols)

	var i uint32
	for i = 0; i < nrows; i++ {
		var seed uint32
		seed = s + i
		row := matrix.Row(i)
		for j := range row {
			seed = LCG_A*seed + LCG_C
			row[j] = byte(seed%100) % 100
		}
	}

	return matrix
}

func main() {
	flag.Parse()

	matrix := randmat(uint32(*nrows), uint32(*ncols), uint32(*seed))

	if !*is_bench {
		for i := uint32(0); i < uint32(*nrows); i++ {
			row := matrix.Row(i)
			for j := range row {
				fmt.Printf("%d ", row[j])
			}
			fmt.Printf("\n")
		}
		fmt.Printf("\n")
	}
}
