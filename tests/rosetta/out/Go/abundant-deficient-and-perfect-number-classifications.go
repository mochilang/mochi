//go:build ignore

package main

import (
	"fmt"
)

// line 4
func pfacSum(i int) int {
	var sum int = 0
	var p int = 1
	for {
		if !(float64(p) <= (float64(i) / float64(2))) {
			break
		}
		if (i % p) == 0 {
			sum = (sum + p)
		}
		p = (p + 1)
	}
	return sum
}

// line 16
func main() {
	var d int = 0
	var a int = 0
	var pnum int = 0
	var i int = 1
	for {
		if !(i <= 20000) {
			break
		}
		var j int = pfacSum(i)
		if j < i {
			d = (d + 1)
		}
		if j == i {
			pnum = (pnum + 1)
		}
		if j > i {
			a = (a + 1)
		}
		i = (i + 1)
	}
	fmt.Println("There are " + fmt.Sprint(d) + " deficient numbers between 1 and 20000")
	fmt.Println("There are " + fmt.Sprint(a) + " abundant numbers  between 1 and 20000")
	fmt.Println("There are " + fmt.Sprint(pnum) + " perfect numbers between 1 and 20000")
}

func main() {
	main()
}
