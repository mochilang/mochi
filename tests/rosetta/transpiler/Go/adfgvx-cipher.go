//go:build ignore

// Generated by Mochi v0.10.52 on 2025-08-01 23:17:48 GMT+7
package main

import (
	"encoding/json"
	"fmt"
	"os"
	"runtime"
	"strconv"
	"strings"
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

func _toFloat(v any) float64 {
	switch t := v.(type) {
	case int:
		return float64(t)
	case int64:
		return float64(t)
	case float64:
		return t
	default:
		return 0
	}
}

var adfgvx string

var alphabet string

func shuffleStr(s string) string {
	var arr []string = []string{}
	_ = arr
	var i int = 0
	_ = i
	for i < len(s) {
		arr = append(arr, string([]rune(s)[i:(i+1)]))
		i = (i + 1)
	}
	var j int = (len(arr) - 1)
	_ = j
	for j > 0 {
		var k int = (_now() % (j + 1))
		_ = k
		var tmp string = arr[j]
		_ = tmp
		arr[j] = arr[k]
		arr[k] = tmp
		j = (j - 1)
	}
	var out string = ""
	_ = out
	i = 0
	for i < len(arr) {
		out = (out + arr[i])
		i = (i + 1)
	}
	return out
}

func createPolybius() []string {
	var shuffled string = shuffleStr(alphabet)
	_ = shuffled
	_ = shuffled
	var labels []string = []string{}
	_ = labels
	var li int = 0
	_ = li
	for li < len(adfgvx) {
		labels = append(labels, string([]rune(adfgvx)[li:(li+1)]))
		li = (li + 1)
	}
	fmt.Println("6 x 6 Polybius square:\n")
	fmt.Println("  | A D F G V X")
	fmt.Println("---------------")
	var p []string = []string{}
	_ = p
	var i int = 0
	_ = i
	for i < 6 {
		var row string = string([]rune(shuffled)[(i * 6):((i + 1) * 6)])
		_ = row
		p = append(p, row)
		var line string = (labels[i] + " | ")
		_ = line
		var j int = 0
		_ = j
		for j < 6 {
			line = ((line + string([]rune(row)[j:(j+1)])) + " ")
			j = (j + 1)
		}
		fmt.Println(line)
		i = (i + 1)
	}
	return p
}

func createKey(n int) string {
	if (n < 7) || (n > 12) {
		fmt.Println("Key should be within 7 and 12 letters long.")
	}
	var pool string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
	_ = pool
	var key string = ""
	_ = key
	var i int = 0
	_ = i
	for i < n {
		var idx int = (_now() % len(pool))
		_ = idx
		key = (key + string([]rune(pool)[idx]))
		pool = (string([]rune(pool)[:idx]) + string([]rune(pool)[(idx+1):len(pool)]))
		i = (i + 1)
	}
	fmt.Println(("\nThe key is " + key))
	return key
}

func orderKey(key string) []int {
	var pairs []any = []any{}
	_ = pairs
	var i int = 0
	_ = i
	for i < len(key) {
		pairs = append(pairs, []any{string([]rune(key)[i:(i + 1)]), i})
		i = (i + 1)
	}
	var n int = len(pairs)
	_ = n
	var m int = 0
	_ = m
	for m < n {
		var j int = 0
		_ = j
		for j < (n - 1) {
			if _toFloat(func(v any) []any {
				if v == nil {
					return nil
				}
				if vv, ok := v.([]any); ok {
					return vv
				}
				if arr, ok := v.([]any); ok {
					if len(arr) == 0 {
						return []any{}
					}
					out := make([]any, len(arr))
					for i, x := range arr {
						out[i] = x.(any)
					}
					return out
				}
				return v.([]any)
			}(pairs[j])[0]) > _toFloat(func(v any) []any {
				if v == nil {
					return nil
				}
				if vv, ok := v.([]any); ok {
					return vv
				}
				if arr, ok := v.([]any); ok {
					if len(arr) == 0 {
						return []any{}
					}
					out := make([]any, len(arr))
					for i, x := range arr {
						out[i] = x.(any)
					}
					return out
				}
				return v.([]any)
			}(pairs[(j + 1)])[0]) {
				tmp := pairs[j]
				_ = tmp
				pairs[j] = pairs[(j + 1)]
				pairs[(j + 1)] = tmp
			}
			j = (j + 1)
		}
		m = (m + 1)
	}
	var res []any = []any{}
	_ = res
	i = 0
	for i < n {
		res = append(res, func(v any) []any {
			if v == nil {
				return nil
			}
			if vv, ok := v.([]any); ok {
				return vv
			}
			if arr, ok := v.([]any); ok {
				if len(arr) == 0 {
					return []any{}
				}
				out := make([]any, len(arr))
				for i, x := range arr {
					out[i] = x.(any)
				}
				return out
			}
			return v.([]any)
		}(pairs[i])[1].(int))
		i = (i + 1)
	}
	return func(v any) []int {
		if v == nil {
			return nil
		}
		if vv, ok := v.([]int); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return []int{}
			}
			out := make([]int, len(arr))
			for i, x := range arr {
				out[i] = x.(int)
			}
			return out
		}
		return v.([]int)
	}(res)
}

func encrypt(polybius []string, key string, plainText string) string {
	var labels []string = []string{}
	_ = labels
	var li int = 0
	_ = li
	for li < len(adfgvx) {
		labels = append(labels, string([]rune(adfgvx)[li:(li+1)]))
		li = (li + 1)
	}
	var temp string = ""
	_ = temp
	var i int = 0
	_ = i
	for i < len(plainText) {
		var r int = 0
		_ = r
		for r < 6 {
			var c int = 0
			_ = c
			for c < 6 {
				if string([]rune(polybius[r])[c:(c+1)]) == string([]rune(plainText)[i:(i+1)]) {
					temp = ((temp + strings.Join(labels[r:(r+1)], "")) + strings.Join(labels[c:(c+1)], ""))
				}
				c = (c + 1)
			}
			r = (r + 1)
		}
		i = (i + 1)
	}
	var colLen int = (len(temp) / len(key))
	_ = colLen
	if (len(temp) % len(key)) > 0 {
		colLen = (colLen + 1)
	}
	var table [][]string = [][]string{}
	_ = table
	var rIdx int = 0
	_ = rIdx
	for rIdx < colLen {
		var row []string = []string{}
		_ = row
		var j int = 0
		_ = j
		for j < len(key) {
			row = append(row, "")
			j = (j + 1)
		}
		table = append(table, row)
		rIdx = (rIdx + 1)
	}
	var idx int = 0
	_ = idx
	for idx < len(temp) {
		var row int = (idx / len(key))
		_ = row
		var col int = (idx % len(key))
		_ = col
		table[row][col] = string([]rune(temp)[idx:(idx + 1)])
		idx = (idx + 1)
	}
	var order []int = orderKey(key)
	_ = order
	var cols []string = []string{}
	_ = cols
	var ci int = 0
	_ = ci
	for ci < len(key) {
		var colStr string = ""
		_ = colStr
		var ri int = 0
		_ = ri
		for ri < colLen {
			colStr = (colStr + table[ri][order[ci]])
			ri = (ri + 1)
		}
		cols = append(cols, colStr)
		ci = (ci + 1)
	}
	var result string = ""
	_ = result
	ci = 0
	for ci < len(cols) {
		result = (result + cols[ci])
		if ci < (len(cols) - 1) {
			result = (result + " ")
		}
		ci = (ci + 1)
	}
	return result
}

func indexOf(s string, ch string) int {
	var i int = 0
	_ = i
	for i < len(s) {
		if string([]rune(s)[i:(i+1)]) == ch {
			return i
		}
		i = (i + 1)
	}
	return (0 - 1)
}

func decrypt(polybius []string, key string, cipherText string) string {
	var colStrs []string = []string{}
	_ = colStrs
	var start int = 0
	_ = start
	var i int = 0
	_ = i
	for i <= len(cipherText) {
		if (i == len(cipherText)) || (string([]rune(cipherText)[i]) == " ") {
			colStrs = append(colStrs, string([]rune(cipherText)[start:i]))
			start = (i + 1)
		}
		i = (i + 1)
	}
	var maxColLen int = 0
	_ = maxColLen
	i = 0
	for i < len(colStrs) {
		if len(colStrs[i]) > maxColLen {
			maxColLen = len(colStrs[i])
		}
		i = (i + 1)
	}
	var cols [][]string = [][]string{}
	_ = cols
	i = 0
	for i < len(colStrs) {
		var s string = colStrs[i]
		_ = s
		var ls []string = []string{}
		_ = ls
		var j int = 0
		_ = j
		for j < len(s) {
			ls = append(ls, string([]rune(s)[j:(j+1)]))
			j = (j + 1)
		}
		if len(s) < maxColLen {
			var pad []string = []string{}
			_ = pad
			var k int = 0
			_ = k
			for k < maxColLen {
				if k < len(ls) {
					pad = append(pad, ls[k])
				} else {
					pad = append(pad, "")
				}
				k = (k + 1)
			}
			cols = append(cols, pad)
		} else {
			cols = append(cols, ls)
		}
		i = (i + 1)
	}
	var table [][]string = [][]string{}
	_ = table
	var r int = 0
	_ = r
	for r < maxColLen {
		var row []string = []string{}
		_ = row
		var c int = 0
		_ = c
		for c < len(key) {
			row = append(row, "")
			c = (c + 1)
		}
		table = append(table, row)
		r = (r + 1)
	}
	var order []int = orderKey(key)
	_ = order
	r = 0
	for r < maxColLen {
		var c int = 0
		_ = c
		for c < len(key) {
			table[r][order[c]] = cols[c][r]
			c = (c + 1)
		}
		r = (r + 1)
	}
	var temp string = ""
	_ = temp
	r = 0
	for r < len(table) {
		var j int = 0
		_ = j
		for j < len(table[r]) {
			temp = (temp + table[r][j])
			j = (j + 1)
		}
		r = (r + 1)
	}
	var plainText string = ""
	_ = plainText
	var idx int = 0
	_ = idx
	for idx < len(temp) {
		var rIdx int = strings.Index(adfgvx, string([]rune(temp)[idx:(idx+1)]))
		_ = rIdx
		_ = rIdx
		var cIdx int = strings.Index(adfgvx, string([]rune(temp)[(idx+1):(idx+2)]))
		_ = cIdx
		plainText = (plainText + string([]rune(polybius[rIdx])[cIdx]))
		idx = (idx + 2)
	}
	return plainText
}

func mochiMain() {
	var plainText string = "ATTACKAT1200AM"
	_ = plainText
	var polybius []string = createPolybius()
	_ = polybius
	var key string = createKey(9)
	_ = key
	fmt.Println(("\nPlaintext : " + plainText))
	var cipherText string = encrypt(func(v any) []string {
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
	}(polybius), key, plainText)
	_ = cipherText
	fmt.Println(("\nEncrypted : " + cipherText))
	var plainText2 string = decrypt(func(v any) []string {
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
	}(polybius), key, cipherText)
	_ = plainText2
	fmt.Println(("\nDecrypted : " + plainText2))
}

func main() {
	func() {
		var ms runtime.MemStats
		runtime.ReadMemStats(&ms)
		startMem := ms.Alloc
		benchStart := time.Now().UnixNano()
		adfgvx = "ADFGVX"
		alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
		mochiMain()
		runtime.ReadMemStats(&ms)
		endMem := ms.Alloc
		benchEnd := time.Now().UnixNano()
		data := map[string]any{"name": "main", "duration_us": (benchEnd - benchStart) / 1000, "memory_bytes": endMem - startMem}
		out, _ := json.MarshalIndent(data, "", "  ")
		fmt.Println(string(out))
	}()
}
