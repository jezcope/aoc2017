package main

import (
	"fmt"
	"math"
	"os"
)

func abs(n int) int {
	if n < 0 {
		return -n
	}
	return n
}

func min(x, y int) int {
	if x < y {
		return x
	}
	return y
}

func max(x, y int) int {
	if x > y {
		return x
	}
	return y
}

func distance(n int) int {
	x, y := spiral_to_xy(n)

	return abs(x) + abs(y)
}

func spiral_to_xy(n int) (int, int) {
	if n == 1 {
		return 0, 0
	}

	r := int(math.Floor((math.Sqrt(float64(n-1)) + 1) / 2))
	n_r := n - (2*r-1)*(2*r-1)
	o := ((n_r - 1) % (2 * r)) - r + 1
	sector := (n_r - 1) / (2 * r)

	switch sector {
	case 0:
		return r, o
	case 1:
		return -o, r
	case 2:
		return -r, -o
	case 3:
		return o, -r
	}

	return 0, 0
}

func xy_to_spiral(x, y int) int {
	if x == 0 && y == 0 {
		// fmt.Println()
		return 1
	}

	r := max(abs(x), abs(y))
	var s, o, n int

	if x+y > 0 && x-y >= 0 {
		s = 0
	} else if x-y < 0 && x+y >= 0 {
		s = 1
	} else if x+y < 0 && x-y <= 0 {
		s = 2
	} else {
		s = 3
	}

	switch s {
	case 0:
		o = y
	case 1:
		o = -x
	case 2:
		o = -y
	case 3:
		o = x
	}

	n = o + r*(2*s+1) + (2*r-1)*(2*r-1)

	return n
}

func get_spiral(mem []int, x, y int) int {
	n := xy_to_spiral(x, y) - 1
	if n < len(mem) {
		return mem[n]
	}

	return 0
}

func stress_test(input int) int {
	mem := make([]int, 1)
	n := 0
	mem[0] = 1

	for mem[n] < input {
		n++
		x, y := spiral_to_xy(n + 1)
		mem = append(mem,
			get_spiral(mem, x+1, y)+
				get_spiral(mem, x+1, y+1)+
				get_spiral(mem, x, y+1)+
				get_spiral(mem, x-1, y+1)+
				get_spiral(mem, x-1, y)+
				get_spiral(mem, x-1, y-1)+
				get_spiral(mem, x, y-1)+
				get_spiral(mem, x+1, y-1))
	}

	return mem[n]
}

func main() {
	var n int
	fmt.Sscanf(os.Args[1], "%d", &n)

	fmt.Printf("Input is %d\n", n)
	fmt.Printf("Distance is %d\n", distance(n))
	fmt.Printf("Stress test result is %d\n", stress_test(n))
}
