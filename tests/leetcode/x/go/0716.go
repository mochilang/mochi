package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

type MaxStack struct{ stack []int }

func (ms *MaxStack) Push(x int) { ms.stack = append(ms.stack, x) }
func (ms *MaxStack) Pop() int {
	n := len(ms.stack) - 1
	x := ms.stack[n]
	ms.stack = ms.stack[:n]
	return x
}
func (ms *MaxStack) Top() int { return ms.stack[len(ms.stack)-1] }
func (ms *MaxStack) PeekMax() int {
	best := ms.stack[0]
	for _, x := range ms.stack[1:] {
		if x > best {
			best = x
		}
	}
	return best
}
func (ms *MaxStack) PopMax() int {
	maxValue := ms.PeekMax()
	buf := []int{}
	for ms.Top() != maxValue {
		buf = append(buf, ms.Pop())
	}
	ms.Pop()
	for i := len(buf) - 1; i >= 0; i-- {
		ms.Push(buf[i])
	}
	return maxValue
}

func main() {
	in := bufio.NewReader(os.Stdin)
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()
	var t int
	if _, err := fmt.Fscan(in, &t); err != nil {
		return
	}
	cases := []string{}
	for ; t > 0; t-- {
		var ops int
		fmt.Fscan(in, &ops)
		var ms *MaxStack
		res := []string{}
		for i := 0; i < ops; i++ {
			var op string
			fmt.Fscan(in, &op)
			switch op {
			case "C":
				ms = &MaxStack{}
				res = append(res, "null")
			case "P":
				var x int
				fmt.Fscan(in, &x)
				ms.Push(x)
				res = append(res, "null")
			case "O":
				res = append(res, fmt.Sprint(ms.Pop()))
			case "T":
				res = append(res, fmt.Sprint(ms.Top()))
			case "M":
				res = append(res, fmt.Sprint(ms.PeekMax()))
			case "X":
				res = append(res, fmt.Sprint(ms.PopMax()))
			}
		}
		cases = append(cases, "["+strings.Join(res, ",")+"]")
	}
	fmt.Fprint(out, strings.Join(cases, "\n\n"))
}
