//go:build ignore

package main

import (
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
	fmt.Println(_sprint("--- Cross Join of three lists ---"))
	for _, c := range combos {
		fmt.Println(strings.TrimRight(strings.Join([]string{_sprint(c.N), _sprint(c.L), _sprint(c.B)}, " "), " "))
	}
}

func _sprint(v any) string {
	if v == nil {
		return "<nil>"
	}
	rv := reflect.ValueOf(v)
	if (rv.Kind() == reflect.Map || rv.Kind() == reflect.Slice) && rv.IsNil() {
		return "<nil>"
	}
	return fmt.Sprint(v)
}
