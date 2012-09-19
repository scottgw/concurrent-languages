/*
 * outer: outer product
 *
 * input:
 *   vector: a vector of (x, y) points
 *   nelts: the number of points
 *
 * output:
 *   matrix: a real matrix, whose values are filled with inter-point
 *     distances
 *   vector: a real vector, whose values are filled with origin-to-point
 *     distances
 */
package main

import (
	"flag"
	"fmt"
	"math"
)

var is_bench = flag.Bool("is_bench", false, "")

var points [10000]int
//var matrix [10000][10000]float64
//var vector [10000]float64

type Point struct {
  x, y int
}

func Max(a, b float64) float64 {
	if a > b {
		return a
	}
	return b
}

func Sqr(x float64) float64 {
	return x * x
}

func Distance(ax, ay, bx, by int) float64 {
	return math.Sqrt(float64(Sqr(float64(ax-bx)) + Sqr(float64(ay-by))))
}

func ToCoord(v, width int) (x, y int) {
	 x, y = v/width, v%width
   return
}

func Outer(wp []Point, nelts int) (m [][]float64, vec []float64) {
	m = make([][]float64, nelts)
  vec = make ([]float64, nelts)
	for i, v := range wp {
    m [i] = make ([]float64, nelts)
		nmax := float64(0)
		vx, vy := ToCoord(v, nelts)
		for j, w := range wp {
			wx, wy := ToCoord(w, nelts)
			if i != j {
        fmt.Printf("%d-(%d,%d) %d-(%d,%d)\n", v, vx, vy, w, wx, wy)
				d := Distance(vx, vy, wx, wy)
        nmax = Max (nmax, d)
        m [i][j] = d
			}
		}

    m [i][i] = float64(nelts) * nmax
    vec [i] = Distance (0, 0, vx, vy)
	}
  return
}

func read_integer() int {
	var value int
	for true {
		var read, _ = fmt.Scanf("%d", &value)
		if read == 1 {
			break
		}
	}
	return value
}

func read_vector_of_points(nelts int) {
	for i := 0; i < nelts; i++ {
		a := read_integer()
		b := read_integer()
		points[i] = Point {a, b}
	}
}

func main() {
	var nelts int

	flag.Parse()

	nelts = read_integer()

	if !*is_bench {
		read_vector_of_points(nelts)
	}

	matrix, vector := Outer(points[0:nelts], nelts)

	if !*is_bench {
		fmt.Printf("%d %d\n", nelts, nelts)
		for i := 0; i < nelts; i++ {
			for j := 0; j < nelts; j++ {
				fmt.Printf("%g ", matrix[i][j])
			}
			fmt.Printf("\n")
		}
		fmt.Printf("\n")

		fmt.Printf("%d\n", nelts)
		for i := 0; i < nelts; i++ {
			fmt.Printf("%g ", vector[i])
		}
		fmt.Printf("\n")
	}
}
