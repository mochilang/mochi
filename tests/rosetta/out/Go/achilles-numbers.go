//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
)

// line 4
func pow10(exp int) int {
	var n int = 1
	var i int = 0
	for {
		if !(i < exp) {
			break
		}
		n = (n * 10)
		i = (i + 1)
	}
	return n
}

// line 14
func totient(n int) int {
	var tot int = n
	var nn int = n
	var i int = 2
	for {
		if !((i * i) <= nn) {
			break
		}
		if (nn % i) == 0 {
			for {
				if !((nn % i) == 0) {
					break
				}
				nn = int(int(_cast[int]((float64(nn) / float64(i)))))
			}
			tot = int(int(_cast[int]((float64(tot) - (float64(tot) / float64(i))))))
		}
		if i == 2 {
			i = 1
		}
		i = (i + 2)
	}
	if nn > 1 {
		tot = int(int(_cast[int]((float64(tot) - (float64(tot) / float64(nn))))))
	}
	return tot
}

// line 38
func getPerfectPowers(maxExp int) {
	var upper int = pow10(maxExp)
	var i int = 2
	for {
		if !((i * i) < upper) {
			break
		}
		var p int = (i * i)
		for {
			p = (p * i)
			if p >= upper {
				break
			}
			pps[p] = true
		}
		i = (i + 1)
	}
}

// line 52
func getAchilles(minExp int, maxExp int) map[int]bool {
	var lower int = pow10(minExp)
	var upper int = pow10(maxExp)
	var achilles map[int]bool = map[int]bool{}
	var b int = 1
	for {
		if !(((b * b) * b) < upper) {
			break
		}
		var b3 int = ((b * b) * b)
		var a int = 1
		for {
			var p int = ((b3 * a) * a)
			if p >= upper {
				break
			}
			if p >= lower {
				_tmp0 := p
				_tmp1 := pps
				_, _tmp2 := _tmp1[_tmp0]
				if !(_tmp2) {
					achilles[p] = true
				}
			}
			a = (a + 1)
		}
		b = (b + 1)
	}
	return achilles
}

// line 75
func sortInts(xs []int) []int {
	var res []int = []int{}
	var tmp []int = xs
	for {
		if !(len(tmp) > 0) {
			break
		}
		var min int = tmp[0]
		var idx int = 0
		var i int = 1
		for {
			if !(i < len(tmp)) {
				break
			}
			if tmp[i] < min {
				min = tmp[i]
				idx = i
			}
			i = (i + 1)
		}
		res = append(append([]int{}, res...), []int{min}...)
		var out []int = []int{}
		var j int = 0
		for {
			if !(j < len(tmp)) {
				break
			}
			if j != idx {
				out = append(append([]int{}, out...), []int{tmp[j]}...)
			}
			j = (j + 1)
		}
		tmp = out
	}
	return res
}

// line 101
func pad(n int, width int) string {
	var s string = fmt.Sprint(n)
	for {
		if !(len(s) < width) {
			break
		}
		s = " " + s
	}
	return s
}

// line 109
func main() {
	var maxDigits int = 15
	getPerfectPowers(maxDigits)
	var achSet map[int]bool = getAchilles(1, 5)
	var ach []int = []int{}
	for _, k := range achSet["keys"]() {
		ach = append(append([]int{}, ach...), []int{_cast[int](k)}...)
	}
	ach = sortInts(ach)
	fmt.Println("First 50 Achilles numbers:")
	var i int = 0
	for {
		if !(i < 50) {
			break
		}
		var line string = ""
		var j int = 0
		for {
			if !(j < 10) {
				break
			}
			line = line + pad(ach[i], 4)
			if j < 9 {
				line = line + " "
			}
			i = (i + 1)
			j = (j + 1)
		}
		fmt.Println(line)
	}
	fmt.Println("\nFirst 30 strong Achilles numbers:")
	var strong []int = []int{}
	var count int = 0
	var idx int = 0
	for {
		if !(count < 30) {
			break
		}
		var tot int = totient(ach[idx])
		_tmp3 := tot
		_tmp4 := achSet
		_, _tmp5 := _tmp4[_tmp3]
		if _tmp5 {
			strong = append(append([]int{}, strong...), []int{ach[idx]}...)
			count = (count + 1)
		}
		idx = (idx + 1)
	}
	i = 0
	for {
		if !(i < 30) {
			break
		}
		var line string = ""
		var j int = 0
		for {
			if !(j < 10) {
				break
			}
			line = line + pad(strong[i], 5)
			if j < 9 {
				line = line + " "
			}
			i = (i + 1)
			j = (j + 1)
		}
		fmt.Println(line)
	}
	fmt.Println("\nNumber of Achilles numbers with:")
	var counts []int = []int{
		1,
		12,
		47,
		192,
		664,
		2242,
		7395,
		24008,
		77330,
		247449,
		788855,
		2508051,
		7960336,
		25235383,
	}
	var d int = 2
	for {
		if !(d <= maxDigits) {
			break
		}
		var c int = counts[(d - 2)]
		fmt.Println(pad(d, 2) + " digits: " + fmt.Sprint(c))
		d = (d + 1)
	}
}

var pps map[int]bool

func main() {
	pps = map[int]bool{}
	main()
}

func _cast[T any](v any) T {
	if tv, ok := v.(T); ok {
		return tv
	}
	var out T
	switch any(out).(type) {
	case int:
		switch vv := v.(type) {
		case int:
			return any(vv).(T)
		case float64:
			return any(int(vv)).(T)
		case float32:
			return any(int(vv)).(T)
		}
	case float64:
		switch vv := v.(type) {
		case int:
			return any(float64(vv)).(T)
		case float64:
			return any(vv).(T)
		case float32:
			return any(float64(vv)).(T)
		}
	case float32:
		switch vv := v.(type) {
		case int:
			return any(float32(vv)).(T)
		case float64:
			return any(float32(vv)).(T)
		case float32:
			return any(vv).(T)
		}
	}
	if m, ok := v.(map[any]any); ok {
		v = _convertMapAny(m)
	}
	data, err := json.Marshal(v)
	if err != nil {
		panic(err)
	}
	if err := json.Unmarshal(data, &out); err != nil {
		panic(err)
	}
	return out
}

func _convertMapAny(m map[any]any) map[string]any {
	out := make(map[string]any, len(m))
	for k, v := range m {
		key := fmt.Sprint(k)
		if sub, ok := v.(map[any]any); ok {
			out[key] = _convertMapAny(sub)
		} else {
			out[key] = v
		}
	}
	return out
}
