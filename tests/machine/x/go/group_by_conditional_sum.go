//go:build ignore

package main

import (
	"fmt"
	"mochi/runtime/data"
	"reflect"
	"sort"
	"strings"
)

func main() {
	type ItemsItem struct {
		Cat  string `json:"cat"`
		Val  int    `json:"val"`
		Flag bool   `json:"flag"`
	}

	var items []ItemsItem = []ItemsItem{ItemsItem{
		Cat:  "a",
		Val:  10,
		Flag: true,
	}, ItemsItem{
		Cat:  "a",
		Val:  5,
		Flag: false,
	}, ItemsItem{
		Cat:  "b",
		Val:  20,
		Flag: true,
	}}
	type Result struct {
		Cat   any     `json:"cat"`
		Share float64 `json:"share"`
	}

	var result []Result = func() []Result {
		groups := map[string]*data.Group{}
		order := []string{}
		for _, i := range items {
			key := i.Cat
			ks := fmt.Sprint(key)
			g, ok := groups[ks]
			if !ok {
				g = &data.Group{Key: key}
				groups[ks] = g
				order = append(order, ks)
			}
			_item := map[string]any{}
			for k, v := range _toAnyMap(i) {
				_item[k] = v
			}
			_item["i"] = i
			g.Items = append(g.Items, _item)
		}
		items := []*data.Group{}
		for _, ks := range order {
			items = append(items, groups[ks])
		}
		type pair struct {
			item *data.Group
			key  any
		}
		pairs := make([]pair, len(items))
		for idx, it := range items {
			g := it
			pairs[idx] = pair{item: it, key: g.Key}
		}
		sort.Slice(pairs, func(i, j int) bool {
			a, b := pairs[i].key, pairs[j].key
			switch av := a.(type) {
			case int:
				switch bv := b.(type) {
				case int:
					return av < bv
				case float64:
					return float64(av) < bv
				}
			case float64:
				switch bv := b.(type) {
				case int:
					return av < float64(bv)
				case float64:
					return av < bv
				}
			case string:
				bs, _ := b.(string)
				return av < bs
			}
			return fmt.Sprint(a) < fmt.Sprint(b)
		})
		for idx, p := range pairs {
			items[idx] = p.item
		}
		results := []Result{}
		for _, g := range items {
			results = append(results, Result{
				Cat: g.Key,
				Share: (float64(_sum(func() []any {
					results := []any{}
					for _, x := range g.Items {
						results = append(results, func() any {
							if _exists(_toAnyMap(x)["flag"]) {
								return _toAnyMap(x)["val"]
							} else {
								return 0
							}
						}())
					}
					return results
				}())) / float64(_sum(func() []any {
					results := []any{}
					for _, x := range g.Items {
						results = append(results, _toAnyMap(x)["val"])
					}
					return results
				}()))),
			})
		}
		return results
	}()
	fmt.Println(result)
}

func _exists(v any) bool {
	if g, ok := v.(*data.Group); ok {
		return len(g.Items) > 0
	}
	switch s := v.(type) {
	case []any:
		return len(s) > 0
	case []int:
		return len(s) > 0
	case []float64:
		return len(s) > 0
	case []string:
		return len(s) > 0
	case []bool:
		return len(s) > 0
	case []map[string]any:
		return len(s) > 0
	case map[string]any:
		return len(s) > 0
	case map[string]int:
		return len(s) > 0
	case string:
		return len([]rune(s)) > 0
	}
	rv := reflect.ValueOf(v)
	switch rv.Kind() {
	case reflect.Slice, reflect.Array:
		return rv.Len() > 0
	case reflect.Map:
		return !rv.IsNil() && rv.Len() > 0
	case reflect.Pointer:
		return !rv.IsNil()
	case reflect.Struct:
		return !rv.IsZero()
	}
	return false
}

func _sum(v any) float64 {
	var items []any
	if g, ok := v.(*data.Group); ok {
		items = g.Items
	} else {
		switch s := v.(type) {
		case []any:
			items = s
		case []int:
			items = []any{}
			for _, v := range s {
				items = append(items, v)
			}
		case []float64:
			items = []any{}
			for _, v := range s {
				items = append(items, v)
			}
		case []string, []bool:
			panic("sum() expects numbers")
		default:
			panic("sum() expects list or group")
		}
	}
	var sum float64
	for _, it := range items {
		switch n := it.(type) {
		case int:
			sum += float64(n)
		case int64:
			sum += float64(n)
		case float64:
			sum += n
		default:
			panic("sum() expects numbers")
		}
	}
	return sum
}

func _toAnyMap(m any) map[string]any {
	switch v := m.(type) {
	case map[string]any:
		return v
	case map[string]string:
		out := make(map[string]any, len(v))
		for k, vv := range v {
			out[k] = vv
		}
		return out
	default:
		rv := reflect.ValueOf(v)
		if rv.Kind() == reflect.Struct {
			out := make(map[string]any, rv.NumField())
			rt := rv.Type()
			for i := 0; i < rv.NumField(); i++ {
				name := rt.Field(i).Name
				if tag := rt.Field(i).Tag.Get("json"); tag != "" {
					comma := strings.Index(tag, ",")
					if comma >= 0 {
						tag = tag[:comma]
					}
					if tag != "-" {
						name = tag
					}
				}
				out[name] = rv.Field(i).Interface()
			}
			return out
		}
		return nil
	}
}
