//go:build ignore

// Generated by Mochi v0.10.52 on 2025-08-02 01:03:58 GMT+7
package main

import (
	"encoding/json"
	"fmt"
	"os"
	"runtime"
	"strconv"
	"time"
)

var seededNow bool
var nowSeed int64

func init() {
	if s := os.Getenv("MOCHI_NOW_SEED"); s != "" {
		if v, err := strconv.ParseInt(s, 10, 64); err == nil {
			nowSeed = v
			seededNow = true
		}
	}
}
func _now() int {
	if seededNow {
		nowSeed = (nowSeed*1664525 + 1013904223) % 2147483647
		return int(nowSeed)
	}
	return int(time.Now().UnixNano())
}

func _substr(s string, start, end int) string {
	r := []rune(s)
	if start < 0 {
		start = 0
	}
	if end > len(r) {
		end = len(r)
	}
	if start > len(r) {
		start = len(r)
	}
	if end < start {
		end = start
	}
	return string(r[start:end])
}

func padLeft(s string, w int) string {
	var res string = ""
	_ = res
	var n int = (w - len(s))
	_ = n
	for n > 0 {
		res = (res + " ")
		n = (n - 1)
	}
	return (res + s)
}

func indexOfFrom(s string, ch string, start int) int {
	var i int = start
	_ = i
	for i < len(s) {
		if _substr(s, i, (i+1)) == ch {
			return i
		}
		i = (i + 1)
	}
	return (0 - 1)
}

func containsStr(s string, sub string) bool {
	var i int = 0
	_ = i
	var sl int = len(s)
	_ = sl
	var subl int = len(sub)
	_ = subl
	for i <= (sl - subl) {
		if _substr(s, i, (i+subl)) == sub {
			return true
		}
		i = (i + 1)
	}
	return false
}

func distinct(slist []string) []string {
	var res []string = []string{}
	_ = res
	for _, s := range slist {
		var found bool = false
		_ = found
		for _, r := range res {
			if r == s {
				found = true
				break
			}
		}
		if !found {
			res = append(res, s)
		}
	}
	return res
}

func permutations(xs []string) [][]string {
	if len(xs) <= 1 {
		return [][]string{xs}
	}
	var res [][]string = [][]string{}
	_ = res
	var i int = 0
	_ = i
	for i < len(xs) {
		var rest []string = []string{}
		_ = rest
		var j int = 0
		_ = j
		for j < len(xs) {
			if j != i {
				rest = append(rest, xs[j])
			}
			j = (j + 1)
		}
		var subs [][]string = permutations(func(v any) []string {
			if v == nil {
				return nil
			}
			if vv, ok := v.([]string); ok {
				return vv
			}
			if arr, ok := v.([]any); ok {
				if len(arr) == 0 {
					return []string{}
				}
				out := make([]string, len(arr))
				for i, x := range arr {
					out[i] = x.(string)
				}
				return out
			}
			return v.([]string)
		}(rest))
		_ = subs
		for _, p := range subs {
			var perm []string = []string{xs[i]}
			_ = perm
			var k int = 0
			_ = k
			for k < len(p) {
				perm = append(perm, p[k])
				k = (k + 1)
			}
			res = append(res, perm)
		}
		i = (i + 1)
	}
	return res
}

func headTailOverlap(s1 string, s2 string) int {
	var start int = 0
	_ = start
	for {
		var ix int = indexOfFrom(s1, string([]rune(s2)[0:1]), start)
		_ = ix
		if ix == (0 - 1) {
			return 0
		}
		start = ix
		var sublen int = (len(s1) - start)
		_ = sublen
		if sublen > len(s2) {
			sublen = len(s2)
		}
		if _substr(s2, 0, sublen) == _substr(s1, start, (start+sublen)) {
			return sublen
		}
		start = (start + 1)
	}
}

func deduplicate(slist []string) []string {
	var arr []string = distinct(func(v any) []string {
		if v == nil {
			return nil
		}
		if vv, ok := v.([]string); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return []string{}
			}
			out := make([]string, len(arr))
			for i, x := range arr {
				out[i] = x.(string)
			}
			return out
		}
		return v.([]string)
	}(slist))
	_ = arr
	var filtered []string = []string{}
	_ = filtered
	var i int = 0
	_ = i
	for i < len(arr) {
		var s1 string = arr[i]
		_ = s1
		var within bool = false
		_ = within
		var j int = 0
		_ = j
		for j < len(arr) {
			if (j != i) && containsStr(arr[j], s1) {
				within = true
				break
			}
			j = (j + 1)
		}
		if !within {
			filtered = append(filtered, s1)
		}
		i = (i + 1)
	}
	return filtered
}

func joinAll(ss []string) string {
	var out string = ""
	_ = out
	for _, s := range ss {
		out = (out + s)
	}
	return out
}

func shortestCommonSuperstring(slist []string) string {
	var ss []string = deduplicate(func(v any) []string {
		if v == nil {
			return nil
		}
		if vv, ok := v.([]string); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return []string{}
			}
			out := make([]string, len(arr))
			for i, x := range arr {
				out[i] = x.(string)
			}
			return out
		}
		return v.([]string)
	}(slist))
	_ = ss
	var shortest string = joinAll(func(v any) []string {
		if v == nil {
			return nil
		}
		if vv, ok := v.([]string); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return []string{}
			}
			out := make([]string, len(arr))
			for i, x := range arr {
				out[i] = x.(string)
			}
			return out
		}
		return v.([]string)
	}(ss))
	_ = shortest
	var perms [][]string = permutations(func(v any) []string {
		if v == nil {
			return nil
		}
		if vv, ok := v.([]string); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return []string{}
			}
			out := make([]string, len(arr))
			for i, x := range arr {
				out[i] = x.(string)
			}
			return out
		}
		return v.([]string)
	}(ss))
	_ = perms
	var idx int = 0
	_ = idx
	for idx < len(perms) {
		var perm []string = perms[idx]
		_ = perm
		var sup string = perm[0]
		_ = sup
		var i int = 0
		_ = i
		for i < (len(ss) - 1) {
			var ov int = headTailOverlap(perm[i], perm[(i+1)])
			_ = ov
			sup = (sup + _substr(perm[(i+1)], ov, len(perm[(i+1)])))
			i = (i + 1)
		}
		if len(sup) < len(shortest) {
			shortest = sup
		}
		idx = (idx + 1)
	}
	return shortest
}

func printCounts(seq string) {
	var a int = 0
	_ = a
	var c int = 0
	_ = c
	var g int = 0
	_ = g
	var t int = 0
	_ = t
	var i int = 0
	_ = i
	for i < len(seq) {
		var ch string = _substr(seq, i, (i + 1))
		_ = ch
		if ch == "A" {
			a = (a + 1)
		} else {
			if ch == "C" {
				c = (c + 1)
			} else {
				if ch == "G" {
					g = (g + 1)
				} else {
					if ch == "T" {
						t = (t + 1)
					}
				}
			}
		}
		i = (i + 1)
	}
	var total int = len(seq)
	_ = total
	fmt.Println((("\nNucleotide counts for " + seq) + ":\n"))
	fmt.Println((padLeft("A", 10) + padLeft(fmt.Sprint(a), 12)))
	fmt.Println((padLeft("C", 10) + padLeft(fmt.Sprint(c), 12)))
	fmt.Println((padLeft("G", 10) + padLeft(fmt.Sprint(g), 12)))
	fmt.Println((padLeft("T", 10) + padLeft(fmt.Sprint(t), 12)))
	fmt.Println((padLeft("Other", 10) + padLeft(fmt.Sprint((total-(((a+c)+g)+t))), 12)))
	fmt.Println("  ____________________")
	fmt.Println((padLeft("Total length", 14) + padLeft(fmt.Sprint(total), 8)))
}

func mochiMain() {
	var tests [][]string = [][]string{[]string{"TA", "AAG", "TA", "GAA", "TA"}, []string{"CATTAGGG", "ATTAG", "GGG", "TA"}, []string{"AAGAUGGA", "GGAGCGCAUC", "AUCGCAAUAAGGA"}, []string{"ATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTAT", "GGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGT", "CTATGTTCTTATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA", "TGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC", "AACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTT", "GCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTC", "CGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTTCGATTCTGCTTATAACACTATGTTCT", "TGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC", "CGTAAAAAATTACAACGTCCTTTGGCTATCTCTTAAACTCCTGCTAAATGCTCGTGC", "GATGGAGCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTTCGATT", "TTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC", "CTATGTTCTTATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA", "TCTCTTAAACTCCTGCTAAATGCTCGTGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGA"}}
	_ = tests
	for _, seqs := range tests {
		var scs string = shortestCommonSuperstring(func(v any) []string {
			if v == nil {
				return nil
			}
			if vv, ok := v.([]string); ok {
				return vv
			}
			if arr, ok := v.([]any); ok {
				if len(arr) == 0 {
					return []string{}
				}
				out := make([]string, len(arr))
				for i, x := range arr {
					out[i] = x.(string)
				}
				return out
			}
			return v.([]string)
		}(seqs))
		_ = scs
		printCounts(scs)
	}
}

func main() {
	func() {
		var ms runtime.MemStats
		runtime.ReadMemStats(&ms)
		startMem := ms.Alloc
		benchStart := time.Now().UnixNano()
		mochiMain()
		runtime.ReadMemStats(&ms)
		endMem := ms.Alloc
		benchEnd := time.Now().UnixNano()
		data := map[string]any{"name": "main", "duration_us": (benchEnd - benchStart) / 1000, "memory_bytes": endMem - startMem}
		out, _ := json.MarshalIndent(data, "", "  ")
		fmt.Println(string(out))
	}()
}
