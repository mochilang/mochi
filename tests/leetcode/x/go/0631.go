package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

type cell struct{ r, c int }
type entry struct {
	kind int
	val  int
	refs map[cell]int
}
type excel struct{ cells map[cell]entry }

func newExcel(_ int, _ string) *excel { return &excel{cells: map[cell]entry{}} }
func (e *excel) set(r int, c string, v int) { e.cells[cell{r, int(c[0] - 'A')}] = entry{kind: 0, val: v} }
func parseCell(s string) cell { n, _ := strconv.Atoi(s[1:]); return cell{n, int(s[0] - 'A')} }
func (e *excel) get(r int, c string) int {
	k := cell{r, int(c[0] - 'A')}
	it, ok := e.cells[k]
	if !ok {
		return 0
	}
	if it.kind == 0 {
		return it.val
	}
	sum := 0
	for ref, cnt := range it.refs {
		sum += e.get(ref.r, string(rune('A'+ref.c))) * cnt
	}
	return sum
}
func (e *excel) sum(r int, c string, nums []string) int {
	refs := map[cell]int{}
	for _, s := range nums {
		colon := -1
		for i := 0; i < len(s); i++ {
			if s[i] == ':' {
				colon = i
				break
			}
		}
		if colon < 0 {
			refs[parseCell(s)]++
			continue
		}
		a, b := parseCell(s[:colon]), parseCell(s[colon+1:])
		for cc := a.c; cc <= b.c; cc++ {
			for rr := a.r; rr <= b.r; rr++ {
				refs[cell{rr, cc}]++
			}
		}
	}
	e.cells[cell{r, int(c[0] - 'A')}] = entry{kind: 1, refs: refs}
	return e.get(r, c)
}
func main() {
	in := bufio.NewReader(os.Stdin)
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()
	var t int
	if _, err := fmt.Fscan(in, &t); err != nil {
		return
	}
	for tc := 0; tc < t; tc++ {
		var q int
		fmt.Fscan(in, &q)
		var ex *excel
		ans := make([]string, 0, q)
		for i := 0; i < q; i++ {
			var op string
			fmt.Fscan(in, &op)
			switch op {
			case "C":
				var h int
				var w string
				fmt.Fscan(in, &h, &w)
				ex = newExcel(h, w)
				ans = append(ans, "null")
			case "S":
				var r, v int
				var c string
				fmt.Fscan(in, &r, &c, &v)
				ex.set(r, c, v)
				ans = append(ans, "null")
			case "G":
				var r int
				var c string
				fmt.Fscan(in, &r, &c)
				ans = append(ans, fmt.Sprint(ex.get(r, c)))
			default:
				var r, k int
				var c string
				fmt.Fscan(in, &r, &c, &k)
				nums := make([]string, k)
				for j := 0; j < k; j++ {
					fmt.Fscan(in, &nums[j])
				}
				ans = append(ans, fmt.Sprint(ex.sum(r, c, nums)))
			}
		}
		if tc > 0 {
			fmt.Fprintln(out)
			fmt.Fprintln(out)
		}
		fmt.Fprint(out, "["+ans[0])
		for i := 1; i < len(ans); i++ {
			fmt.Fprint(out, ","+ans[i])
		}
		fmt.Fprint(out, "]")
	}
}
