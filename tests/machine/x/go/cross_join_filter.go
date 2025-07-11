//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
	"reflect"
	"strings"
)

func main() {
	var nums []int = []int{1, 2, 3}
	var letters []string = []string{"A", "B"}
	_ = letters
	type Pairs struct {
		N any `json:"n"`
		L any `json:"l"`
	}

	var pairs []Pairs = func() []Pairs {
		_res := []Pairs{}
		for _, n := range nums {
			if (n % 2) == 0 {
				for _, l := range letters {
					_res = append(_res, Pairs{
						N: n,
						L: l,
					})
				}
			}
		}
		return _res
	}()
	fmt.Println(_fmt("--- Even pairs ---"))
	for _, p := range pairs {
		fmt.Println(strings.TrimRight(strings.Join([]string{_fmt(p.N), _fmt(p.L)}, " "), " "))
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
