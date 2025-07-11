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
		Cat string `json:"cat"`
		Val int    `json:"val"`
	}

	var items []ItemsItem = []ItemsItem{
		ItemsItem{
			Cat: "a",
			Val: 3,
		},
		ItemsItem{
			Cat: "a",
			Val: 1,
		},
		ItemsItem{
			Cat: "b",
			Val: 5,
		},
		ItemsItem{
			Cat: "b",
			Val: 2,
		},
	}
	type Grouped struct {
		Cat   any `json:"cat"`
		Total int `json:"total"`
	}

	var grouped []Grouped = func() []Grouped {
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
			pairs[idx] = pair{item: it, key: -_sum(func() []any {
				results := []any{}
				for _, x := range g.Items {
					results = append(results, (x).(map[string]any)["val"])
				}
				return results
			}())}
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
		results := []Grouped{}
		for _, g := range items {
			results = append(results, Grouped{
				Cat: g.Key,
				Total: _sum(func() []any {
					results := []any{}
					for _, x := range g.Items {
						results = append(results, (x).(map[string]any)["val"])
					}
					return results
				}()),
			})
		}
		return results
	}()
	fmt.Println(strings.TrimSuffix(strings.TrimPrefix(fmt.Sprint(grouped), "["), "]"))
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
