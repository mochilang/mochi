//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
	"reflect"
	"strings"
)

func main() {
	var nums []int = []int{1, 2}
	var letters []string = []string{"A", "B"}
	_ = letters
	var bools []bool = []bool{true, false}
	_ = bools
	type Combos struct {
		N any `json:"n"`
		L any `json:"l"`
		B any `json:"b"`
	}

	var combos []Combos = func() []Combos {
		_res := []Combos{}
		for _, n := range nums {
			for _, l := range letters {
				for _, b := range bools {
					_res = append(_res, Combos{
						N: n,
						L: l,
						B: b,
					})
				}
			}
		}
		return _res
	}()
	fmt.Println(_fmt("--- Cross Join of three lists ---"))
	for _, c := range combos {
		fmt.Println(strings.TrimRight(strings.Join([]string{_fmt(c.N), _fmt(c.L), _fmt(c.B)}, " "), " "))
	}
}

func _fmt(v any) string {
	if v == nil {
		return "<nil>"
	}
	rv := reflect.ValueOf(v)
	if rv.Kind() == reflect.Pointer {
		if rv.IsNil() {
			return "<nil>"
		}
		v = rv.Elem().Interface()
		rv = reflect.ValueOf(v)
	}
	if rv.Kind() == reflect.Struct {
		if rv.IsZero() {
			return "<nil>"
		}
		b, _ := json.Marshal(v)
		var m map[string]any
		_ = json.Unmarshal(b, &m)
		return fmt.Sprint(m)
	}
	return fmt.Sprint(v)
}
