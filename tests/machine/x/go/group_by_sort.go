package main

import (
	"encoding/json"
	"fmt"
	"mochi/runtime/data"
	"sort"
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
	var grouped []map[string]any = func() []map[string]any {
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
			for k, v := range _cast[map[string]any](i) {
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
				_res := []any{}
				for _, x := range g.Items {
					_res = append(_res, _cast[map[string]any](x)["val"])
				}
				return _res
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
		_res := []map[string]any{}
		for _, g := range items {
			_res = append(_res, map[string]any{"cat": g.Key, "total": _sum(func() []any {
				_res := []any{}
				for _, x := range g.Items {
					_res = append(_res, _cast[map[string]any](x)["val"])
				}
				return _res
			}())})
		}
		return _res
	}()
	fmt.Println(grouped)
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
