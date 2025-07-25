//go:build ignore

// Generated by Mochi compiler v0.10.30 on 2006-01-02T15:04:05Z

package main

import (
	"fmt"
	"os"
	"reflect"
	"strconv"
	"strings"
	"time"
)

type v map[string]any

type Node struct {
	Op    int            `json:"op"`
	Left  map[string]any `json:"left"`
	Right map[string]any `json:"right"`
}

// line 10
func newNum(n int) map[string]any {
	return map[string]any{"op": OP_NUM, "value": map[string]int{"num": n, "denom": 1}}
}

// line 14
func exprEval(x map[string]any) map[string]int {
	if _equal(x["op"], OP_NUM) {
		return (x["value"]).(map[string]int)
	}
	var l map[string]int = exprEval((x["left"]).(map[string]any))
	var r map[string]int = exprEval((x["right"]).(map[string]any))
	if _equal(x["op"], OP_ADD) {
		return map[string]int{"num": ((l["num"] * r["denom"]) + (l["denom"] * r["num"])), "denom": (l["denom"] * r["denom"])}
	}
	if _equal(x["op"], OP_SUB) {
		return map[string]int{"num": ((l["num"] * r["denom"]) - (l["denom"] * r["num"])), "denom": (l["denom"] * r["denom"])}
	}
	if _equal(x["op"], OP_MUL) {
		return map[string]int{"num": (l["num"] * r["num"]), "denom": (l["denom"] * r["denom"])}
	}
	return map[string]int{"num": (l["num"] * r["denom"]), "denom": (l["denom"] * r["num"])}
}

// line 31
func exprString(x map[string]any) string {
	if _equal(x["op"], OP_NUM) {
		return fmt.Sprint((x["value"]).(map[string]any)["num"])
	}
	var ls string = exprString((x["left"]).(map[string]any))
	var rs string = exprString((x["right"]).(map[string]any))
	var opstr string = ""
	if _equal(x["op"], OP_ADD) {
		opstr = " + "
	} else if _equal(x["op"], OP_SUB) {
		opstr = " - "
	} else if _equal(x["op"], OP_MUL) {
		opstr = " * "
	} else {
		opstr = " / "
	}
	return "(" + ls + opstr + rs + ")"
}

// line 47
func solve(xs []map[string]any) bool {
	if len(xs) == 1 {
		var f map[string]int = exprEval(xs[0])
		if (f["denom"] != 0) && (f["num"] == (f["denom"] * goal)) {
			fmt.Println(any(exprString(xs[0])))
			return true
		}
		return false
	}
	var i int = 0
	for i < len(xs) {
		var j int = (i + 1)
		for j < len(xs) {
			var rest []map[string]any = []map[string]any{}
			var k int = 0
			for k < len(xs) {
				if (k != i) && (k != j) {
					rest = append(rest, xs[k])
				}
				k = (k + 1)
			}
			var a map[string]any = xs[i]
			var b map[string]any = xs[j]
			for _, op := range []int{
				OP_ADD,
				OP_SUB,
				OP_MUL,
				OP_DIV,
			} {
				var node Node = Node{
					Op:    op,
					Left:  a,
					Right: b,
				}
				if solve(_convSlice[any, map[string]any](append(rest, map[string]any(node)))) {
					return true
				}
			}
			var node Node = Node{
				Op:    OP_SUB,
				Left:  b,
				Right: a,
			}
			if solve(_convSlice[any, map[string]any](append(rest, map[string]any(node)))) {
				return true
			}
			node = Node{
				Op:    OP_DIV,
				Left:  b,
				Right: a,
			}
			if solve(_convSlice[any, map[string]any](append(rest, map[string]any(node)))) {
				return true
			}
			j = (j + 1)
		}
		i = (i + 1)
	}
	return false
}

// line 83
func mainFn() {
	var iter int = 0
	for iter < 10 {
		var cards []map[string]any = []map[string]any{}
		var i int = 0
		for i < n_cards {
			var n int64 = (int64((int64(_now()) % int64((digit_range - 1)))) + int64(1))
			cards = append(cards, newNum(n))
			fmt.Println(any(" " + fmt.Sprint(any(n))))
			i = (i + 1)
		}
		fmt.Println(any(":  "))
		if !(solve(cards)) {
			fmt.Println(any("No solution"))
		}
		iter = (iter + 1)
	}
}

func main() {
	var OP_NUM int = 0
	_ = OP_NUM
	var OP_ADD int = 1
	_ = OP_ADD
	var OP_SUB int = 2
	_ = OP_SUB
	var OP_MUL int = 3
	_ = OP_MUL
	var OP_DIV int = 4
	_ = OP_DIV
	var n_cards int = 4
	_ = n_cards
	var goal int = 24
	_ = goal
	var digit_range int = 9
	_ = digit_range
	mainFn()
}

func _convSlice[T any, U any](s []T) []U {
	out := make([]U, len(s))
	for i, v := range s {
		out[i] = any(v).(U)
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

func _copyToMap(dst map[string]any, src any) {
	switch m := src.(type) {
	case map[string]any:
		for k, v := range m {
			dst[k] = v
		}
	case map[string]string:
		for k, v := range m {
			dst[k] = v
		}
	case map[any]any:
		for k, v := range _convertMapAny(m) {
			dst[k] = v
		}
	default:
		rv := reflect.ValueOf(m)
		if rv.Kind() == reflect.Struct {
			rt := rv.Type()
			for i := 0; i < rv.NumField(); i++ {
				name := rt.Field(i).Name
				if tag := rt.Field(i).Tag.Get("json"); tag != "" {
					if c := strings.Index(tag, ","); c >= 0 {
						tag = tag[:c]
					}
					if tag != "-" {
						name = tag
					}
				}
				dst[name] = rv.Field(i).Interface()
			}
		}
	}
}

func _equal(a, b any) bool {
	av := reflect.ValueOf(a)
	bv := reflect.ValueOf(b)
	if av.Kind() == reflect.Struct && bv.Kind() == reflect.Map {
		am := map[string]any{}
		_copyToMap(am, a)
		bm := map[string]any{}
		_copyToMap(bm, b)
		return _equal(am, bm)
	}
	if av.Kind() == reflect.Map && bv.Kind() == reflect.Struct {
		am := map[string]any{}
		_copyToMap(am, a)
		bm := map[string]any{}
		_copyToMap(bm, b)
		return _equal(am, bm)
	}
	if av.Kind() == reflect.Slice && bv.Kind() == reflect.Slice {
		if av.Len() != bv.Len() {
			return false
		}
		for i := 0; i < av.Len(); i++ {
			if !_equal(av.Index(i).Interface(), bv.Index(i).Interface()) {
				return false
			}
		}
		return true
	}
	if av.Kind() == reflect.Map && bv.Kind() == reflect.Map {
		if av.Len() != bv.Len() {
			return false
		}
		for _, k := range av.MapKeys() {
			bvVal := bv.MapIndex(k)
			if !bvVal.IsValid() {
				return false
			}
			if !_equal(av.MapIndex(k).Interface(), bvVal.Interface()) {
				return false
			}
		}
		return true
	}
	if (av.Kind() == reflect.Int || av.Kind() == reflect.Int64 || av.Kind() == reflect.Float64) &&
		(bv.Kind() == reflect.Int || bv.Kind() == reflect.Int64 || bv.Kind() == reflect.Float64) {
		return av.Convert(reflect.TypeOf(float64(0))).Float() == bv.Convert(reflect.TypeOf(float64(0))).Float()
	}
	return reflect.DeepEqual(a, b)
}

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
func _now() int64 {
	if seededNow {
		nowSeed = (nowSeed*1664525 + 1013904223) % 2147483647
		return nowSeed
	}
	return time.Now().UnixNano()
}
