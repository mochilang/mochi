//go:build ignore

// Generated by Mochi v0.10.55 on 2025-08-02 18:06:49 GMT+7
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

func _split(s, sep string) []string {
	if sep == "" {
		sep = " "
	}
	return strings.Split(s, sep)
}

var small []string

var tens []string

var smallOrd []string

var tensOrd []string

func say(n int) string {
	if n < 20 {
		return small[n]
	}
	if n < 100 {
		var res string = tens[(n / 10)]
		_ = res
		var m int = (n % 10)
		_ = m
		if m != 0 {
			res = ((res + "-") + small[m])
		}
		return res
	}
	if n < 1000 {
		var res string = (say((n / 100)) + " hundred")
		_ = res
		var m int = (n % 100)
		_ = m
		if m != 0 {
			res = ((res + " ") + say(m))
		}
		return res
	}
	if n < 1000000 {
		var res string = (say((n / 1000)) + " thousand")
		_ = res
		var m int = (n % 1000)
		_ = m
		if m != 0 {
			res = ((res + " ") + say(m))
		}
		return res
	}
	var res string = (say((n / 1000000)) + " million")
	_ = res
	var m int = (n % 1000000)
	_ = m
	if m != 0 {
		res = ((res + " ") + say(m))
	}
	return res
}

func sayOrdinal(n int) string {
	if n < 20 {
		return smallOrd[n]
	}
	if n < 100 {
		if (n % 10) == 0 {
			return tensOrd[(n / 10)]
		}
		return ((say((n - (n % 10))) + "-") + smallOrd[(n%10)])
	}
	if n < 1000 {
		if (n % 100) == 0 {
			return (say((n / 100)) + " hundredth")
		}
		return ((say((n / 100)) + " hundred ") + sayOrdinal((n % 100)))
	}
	if n < 1000000 {
		if (n % 1000) == 0 {
			return (say((n / 1000)) + " thousandth")
		}
		return ((say((n / 1000)) + " thousand ") + sayOrdinal((n % 1000)))
	}
	if (n % 1000000) == 0 {
		return (say((n / 1000000)) + " millionth")
	}
	return ((say((n / 1000000)) + " million ") + sayOrdinal((n % 1000000)))
}

func split(s string, sep string) []string {
	var parts []string = []string{}
	_ = parts
	var cur string = ""
	_ = cur
	var i int = 0
	_ = i
	for i < len(s) {
		if ((len(sep) > 0) && ((i + len(sep)) <= len(s))) && (_substr(s, i, (i+len(sep))) == sep) {
			parts = append(parts, cur)
			cur = ""
			i = (i + len(sep))
		} else {
			cur = (cur + _substr(s, i, (i+1)))
			i = (i + 1)
		}
	}
	parts = append(parts, cur)
	return parts
}

func countLetters(s string) int {
	var cnt int = 0
	_ = cnt
	var i int = 0
	_ = i
	for i < len(s) {
		var ch string = _substr(s, i, (i + 1))
		_ = ch
		if ((ch >= "A") && (ch <= "Z")) || ((ch >= "a") && (ch <= "z")) {
			cnt = (cnt + 1)
		}
		i = (i + 1)
	}
	return cnt
}

var words []string

var idx int

func wordLen(w int) []any {
	for len(words) < w {
		idx = (idx + 1)
		var n int = countLetters(words[idx])
		_ = n
		var parts []string = _split(say(n), " ")
		_ = parts
		var j int = 0
		_ = j
		for j < len(parts) {
			words = append(words, parts[j])
			j = (j + 1)
		}
		words = append(words, "in")
		words = append(words, "the")
		parts = _split((sayOrdinal((idx + 1)) + ","), " ")
		j = 0
		for j < len(parts) {
			words = append(words, parts[j])
			j = (j + 1)
		}
	}
	var word string = words[(w - 1)]
	_ = word
	_ = word
	return []any{word, countLetters(word)}
}

func totalLength() int {
	var tot int = 0
	_ = tot
	var i int = 0
	_ = i
	for i < len(words) {
		tot = (tot + len(words[i]))
		if i < (len(words) - 1) {
			tot = (tot + 1)
		}
		i = (i + 1)
	}
	return tot
}

func pad(n int, width int) string {
	var s string = fmt.Sprint(n)
	_ = s
	for len(s) < width {
		s = (" " + s)
	}
	return s
}

func mochiMain() {
	fmt.Println("The lengths of the first 201 words are:")
	var line string = ""
	_ = line
	var i int = 1
	_ = i
	for i <= 201 {
		if (i % 25) == 1 {
			if i != 1 {
				fmt.Println(line)
			}
			line = (pad(i, 3) + ":")
		}
		var r []any = wordLen(i)
		_ = r
		n := r[1]
		_ = n
		line = ((line + " ") + pad(n.(int), 2))
		i = (i + 1)
	}
	fmt.Println(line)
	fmt.Println(("Length of sentence so far: " + fmt.Sprint(totalLength())))
	for _, n := range []int{1000, 10000, 100000, 1000000, 10000000} {
		var r []any = wordLen(n)
		_ = r
		w := r[0]
		_ = w
		l := r[1]
		_ = l
		fmt.Println(((((((("Word " + pad(n, 8)) + " is \"") + fmt.Sprint(w)) + "\", with ") + fmt.Sprint(l)) + " letters.  Length of sentence so far: ") + fmt.Sprint(totalLength())))
	}
}

func main() {
	func() {
		var ms runtime.MemStats
		runtime.ReadMemStats(&ms)
		startMem := ms.Alloc
		benchStart := time.Now().UnixNano()
		small = []string{"zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"}
		tens = []string{"", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"}
		smallOrd = []string{"zeroth", "first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth", "thirteenth", "fourteenth", "fifteenth", "sixteenth", "seventeenth", "eighteenth", "nineteenth"}
		tensOrd = []string{"", "", "twentieth", "thirtieth", "fortieth", "fiftieth", "sixtieth", "seventieth", "eightieth", "ninetieth"}
		words = []string{"Four", "is", "the", "number", "of", "letters", "in", "the", "first", "word", "of", "this", "sentence,"}
		idx = 0
		mochiMain()
		runtime.ReadMemStats(&ms)
		endMem := ms.Alloc
		benchEnd := time.Now().UnixNano()
		data := map[string]any{"name": "main", "duration_us": (benchEnd - benchStart) / 1000, "memory_bytes": endMem - startMem}
		out, _ := json.MarshalIndent(data, "", "  ")
		fmt.Println(string(out))
	}()
}
