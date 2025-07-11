//go:build ignore

package main

import (
	"fmt"
)

// line 4
func neighborsList() [][]int {
	return [][]int{
		[]int{1, 3},
		[]int{0, 2, 4},
		[]int{1, 5},
		[]int{0, 4, 6},
		[]int{
			1,
			3,
			5,
			7,
		},
		[]int{2, 4, 8},
		[]int{3, 7},
		[]int{4, 6, 8},
		[]int{5, 7},
	}
}

// line 18
func plus(a []int, b []int) []int {
	var res []int = []int{}
	var i int = 0
	for {
		if !(i < len(a)) {
			break
		}
		res = append(_convSlice[int, any](res), (a[i] + b[i]))
		i = (i + 1)
	}
	return res
}

// line 28
func isStable(p []int) bool {
	for _, v := range p {
		if v > 3 {
			return false
		}
	}
	return true
}

// line 35
func topple(p *[]int) int {
	var neighbors [][]int = neighborsList()
	var i int = 0
	for {
		if !(i < len(p)) {
			break
		}
		if p[i] > 3 {
			p[i] = (p[i] - 4)
			var nbs []int = neighbors[i]
			for _, j := range nbs {
				p[j] = (p[j] + 1)
			}
			return 0
		}
		i = (i + 1)
	}
	return 0
}

// line 52
func pileString(p []int) string {
	var s string = ""
	var r int = 0
	for {
		if !(r < 3) {
			break
		}
		var c int = 0
		for {
			if !(c < 3) {
				break
			}
			s = s + fmt.Sprint(p[((3*r)+c)]) + " "
			c = (c + 1)
		}
		s = s + "\n"
		r = (r + 1)
	}
	return s
}

func main() {
	fmt.Println("Avalanche of topplings:\n")
	var s4 []int = []int{
		4,
		3,
		3,
		3,
		1,
		2,
		0,
		2,
		3,
	}
	fmt.Println(pileString(s4))
	for {
		if !(!isStable(s4)) {
			break
		}
		topple(s4)
		fmt.Println(pileString(s4))
	}
	fmt.Println("Commutative additions:\n")
	var s1 []int = []int{
		1,
		2,
		0,
		2,
		1,
		1,
		0,
		1,
		3,
	}
	var s2 []int = []int{
		2,
		1,
		3,
		1,
		0,
		1,
		0,
		1,
		0,
	}
	var s3_a []int = plus(s1, s2)
	for {
		if !(!isStable(s3_a)) {
			break
		}
		topple(s3_a)
	}
	var s3_b []int = plus(s2, s1)
	for {
		if !(!isStable(s3_b)) {
			break
		}
		topple(s3_b)
	}
	fmt.Println(pileString(s1) + "\nplus\n\n" + pileString(s2) + "\nequals\n\n" + pileString(s3_a))
	fmt.Println("and\n\n" + pileString(s2) + "\nplus\n\n" + pileString(s1) + "\nalso equals\n\n" + pileString(s3_b))
	fmt.Println("Addition of identity sandpile:\n")
	var s3 []int = []int{
		3,
		3,
		3,
		3,
		3,
		3,
		3,
		3,
		3,
	}
	var s3_id []int = []int{
		2,
		1,
		2,
		1,
		0,
		1,
		2,
		1,
		2,
	}
	var s4b []int = plus(s3, s3_id)
	for {
		if !(!isStable(s4b)) {
			break
		}
		topple(s4b)
	}
	fmt.Println(pileString(s3) + "\nplus\n\n" + pileString(s3_id) + "\nequals\n\n" + pileString(s4b))
	fmt.Println("Addition of identities:\n")
	var s5 []int = plus(s3_id, s3_id)
	for {
		if !(!isStable(s5)) {
			break
		}
		topple(s5)
	}
	fmt.Println(pileString(s3_id) + "\nplus\n\n" + pileString(s3_id) + "\nequals\n\n" + pileString(s5))
}

func _convSlice[T any, U any](s []T) []U {
	out := []U{}
	for _, v := range s {
		out = append(out, any(v).(U))
	}
	return out
}
