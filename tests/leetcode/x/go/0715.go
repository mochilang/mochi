package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

type Interval struct{ l, r int }

type RangeModule struct{ intervals []Interval }

func (rm *RangeModule) AddRange(left, right int) {
	merged := make([]Interval, 0, len(rm.intervals)+1)
	i := 0
	for i < len(rm.intervals) && rm.intervals[i].r < left {
		merged = append(merged, rm.intervals[i])
		i++
	}
	newL, newR := left, right
	for i < len(rm.intervals) && rm.intervals[i].l <= newR {
		if rm.intervals[i].l < newL {
			newL = rm.intervals[i].l
		}
		if rm.intervals[i].r > newR {
			newR = rm.intervals[i].r
		}
		i++
	}
	merged = append(merged, Interval{newL, newR})
	merged = append(merged, rm.intervals[i:]...)
	rm.intervals = merged
}

func (rm *RangeModule) QueryRange(left, right int) bool {
	lo, hi := 0, len(rm.intervals)-1
	for lo <= hi {
		mid := (lo + hi) / 2
		cur := rm.intervals[mid]
		if cur.l <= left {
			if right <= cur.r {
				return true
			}
			lo = mid + 1
		} else {
			hi = mid - 1
		}
	}
	return false
}

func (rm *RangeModule) RemoveRange(left, right int) {
	kept := make([]Interval, 0, len(rm.intervals))
	for _, cur := range rm.intervals {
		if cur.r <= left || right <= cur.l {
			kept = append(kept, cur)
		} else {
			if cur.l < left {
				kept = append(kept, Interval{cur.l, left})
			}
			if right < cur.r {
				kept = append(kept, Interval{right, cur.r})
			}
		}
	}
	rm.intervals = kept
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
		var rm *RangeModule
		res := []string{}
		for i := 0; i < ops; i++ {
			var op string
			fmt.Fscan(in, &op)
			switch op {
			case "C":
				rm = &RangeModule{}
				res = append(res, "null")
			case "A":
				var l, r int
				fmt.Fscan(in, &l, &r)
				rm.AddRange(l, r)
				res = append(res, "null")
			case "R":
				var l, r int
				fmt.Fscan(in, &l, &r)
				rm.RemoveRange(l, r)
				res = append(res, "null")
			case "Q":
				var l, r int
				fmt.Fscan(in, &l, &r)
				if rm.QueryRange(l, r) {
					res = append(res, "true")
				} else {
					res = append(res, "false")
				}
			}
		}
		cases = append(cases, "["+strings.Join(res, ",")+"]")
	}
	fmt.Fprint(out, strings.Join(cases, "\n\n"))
}
