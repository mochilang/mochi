//go:build ignore

// Generated by Mochi compiler v0.10.28 on 1970-01-01T00:00:00Z

package main

import (
	"fmt"
	"mochi/runtime/data"
	"reflect"
	"sort"
	"strings"
)

type v = DataVar

type DataVar struct {
	Tag string `json:"tag"`
	Val int    `json:"val"`
}

func main() {
	dataVar := []DataVar{DataVar{
		Tag: "a",
		Val: 1,
	}, DataVar{
		Tag: "a",
		Val: 2,
	}, DataVar{
		Tag: "b",
		Val: 3,
	}}
	groups := func() []*data.Group {
		groups := map[string]*data.Group{}
		order := []string{}
		for _, d := range dataVar {
			key := d.Tag
			ks := fmt.Sprint(key)
			g, ok := groups[ks]
			if !ok {
				g = &data.Group{Key: key}
				groups[ks] = g
				order = append(order, ks)
			}
			g.Items = append(g.Items, d)
		}
		results := []*data.Group{}
		for _, ks := range order {
			g := groups[ks]
			results = append(results, g)
		}
		return results
	}()
	tmp := []any{}
	for _, g := range groups {
		total := 0
		for _, x := range _convSlice[any, DataVar](g.Items) {
			total = (total + x.Val)
		}
		tmp = append(tmp, any(map[string]any{"tag": g.Key.(string), "total": total}))
	}
	result := func() []any {
		src := tmp
		resAny := _query(src, []_joinSpec{}, _queryOpts{selectFn: func(_a ...any) any { r := _a[0]; _ = r; return r }, sortKey: func(_a ...any) any { r := _a[0]; _ = r; return _getField(r, "tag") }, skip: -1, take: -1})
		out := make([]any, len(resAny))
		for i, v := range resAny {
			out[i] = v
		}
		return out
	}()
	fmt.Println(strings.TrimSpace(strings.Join([]string{strings.Trim(strings.Trim(fmt.Sprint(any(result)), "[]"), " ")}, " ")))
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

func _getField(v any, name string) any {
	switch m := v.(type) {
	case map[string]any:
		return m[name]
	case map[string]string:
		if s, ok := m[name]; ok {
			return s
		}
	case map[any]any:
		return _convertMapAny(m)[name]
	default:
		rv := reflect.ValueOf(m)
		if rv.Kind() == reflect.Struct {
			rt := rv.Type()
			for i := 0; i < rv.NumField(); i++ {
				fn := rt.Field(i)
				field := fn.Name
				if tag := fn.Tag.Get("json"); tag != "" {
					if c := strings.Index(tag, ","); c >= 0 {
						tag = tag[:c]
					}
					if tag != "-" {
						field = tag
					}
				}
				if field == name {
					return rv.Field(i).Interface()
				}
			}
		}
	}
	return nil
}

type _joinSpec struct {
	items    []any
	on       func(...any) bool
	leftKey  func(...any) any
	rightKey func(any) any
	left     bool
	right    bool
}
type _queryOpts struct {
	selectFn func(...any) any
	where    func(...any) bool
	sortKey  func(...any) any
	skip     int
	take     int
}

func _query(src []any, joins []_joinSpec, opts _queryOpts) []any {
	items := make([][]any, len(src))
	for i, v := range src {
		items[i] = []any{v}
	}
	for _, j := range joins {
		if j.leftKey != nil && j.rightKey != nil {
			if j.right && !j.left {
				lmap := map[string][]int{}
				for li, l := range items {
					key := fmt.Sprint(j.leftKey(l...))
					lmap[key] = append(lmap[key], li)
				}
				joined := [][]any{}
				for _, right := range j.items {
					key := fmt.Sprint(j.rightKey(right))
					if is, ok := lmap[key]; ok {
						for _, li := range is {
							left := items[li]
							keep := true
							if j.on != nil {
								args := append(append([]any(nil), left...), right)
								keep = j.on(args...)
							}
							if !keep {
								continue
							}
							joined = append(joined, append(append([]any(nil), left...), right))
						}
					} else {
						undef := make([]any, len(items[0]))
						joined = append(joined, append(undef, right))
					}
				}
				items = joined
				continue
			}
			rmap := map[string][]int{}
			for ri, r := range j.items {
				key := fmt.Sprint(j.rightKey(r))
				rmap[key] = append(rmap[key], ri)
			}
			joined := [][]any{}
			matched := make([]bool, len(j.items))
			for _, left := range items {
				key := fmt.Sprint(j.leftKey(left...))
				if is, ok := rmap[key]; ok {
					m := false
					for _, ri := range is {
						right := j.items[ri]
						keep := true
						if j.on != nil {
							args := append(append([]any(nil), left...), right)
							keep = j.on(args...)
						}
						if !keep {
							continue
						}
						m = true
						matched[ri] = true
						joined = append(joined, append(append([]any(nil), left...), right))
					}
					if j.left && !m {
						joined = append(joined, append(append([]any(nil), left...), nil))
					}
				} else if j.left {
					joined = append(joined, append(append([]any(nil), left...), nil))
				}
			}
			if j.right {
				lw := 0
				if len(items) > 0 {
					lw = len(items[0])
				}
				for ri, right := range j.items {
					if !matched[ri] {
						undef := make([]any, lw)
						joined = append(joined, append(undef, right))
					}
				}
			}
			items = joined
			continue
		}
		joined := [][]any{}
		if j.right && j.left {
			matched := make([]bool, len(j.items))
			for _, left := range items {
				m := false
				for ri, right := range j.items {
					keep := true
					if j.on != nil {
						args := append(append([]any(nil), left...), right)
						keep = j.on(args...)
					}
					if !keep {
						continue
					}
					m = true
					matched[ri] = true
					joined = append(joined, append(append([]any(nil), left...), right))
				}
				if !m {
					joined = append(joined, append(append([]any(nil), left...), nil))
				}
			}
			for ri, right := range j.items {
				if !matched[ri] {
					undef := make([]any, len(items[0]))
					joined = append(joined, append(undef, right))
				}
			}
		} else if j.right {
			for _, right := range j.items {
				m := false
				for _, left := range items {
					keep := true
					if j.on != nil {
						args := append(append([]any(nil), left...), right)
						keep = j.on(args...)
					}
					if !keep {
						continue
					}
					m = true
					joined = append(joined, append(append([]any(nil), left...), right))
				}
				if !m {
					undef := make([]any, len(items[0]))
					joined = append(joined, append(undef, right))
				}
			}
		} else {
			for _, left := range items {
				m := false
				for _, right := range j.items {
					keep := true
					if j.on != nil {
						args := append(append([]any(nil), left...), right)
						keep = j.on(args...)
					}
					if !keep {
						continue
					}
					m = true
					joined = append(joined, append(append([]any(nil), left...), right))
				}
				if j.left && !m {
					joined = append(joined, append(append([]any(nil), left...), nil))
				}
			}
		}
		items = joined
	}
	if opts.where != nil {
		filtered := [][]any{}
		for _, r := range items {
			if opts.where(r...) {
				filtered = append(filtered, r)
			}
		}
		items = filtered
	}
	if opts.sortKey != nil {
		type pair struct {
			item []any
			key  any
		}
		pairs := make([]pair, len(items))
		for i, it := range items {
			pairs[i] = pair{it, opts.sortKey(it...)}
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
		for i, p := range pairs {
			items[i] = p.item
		}
	}
	if opts.skip >= 0 {
		if opts.skip < len(items) {
			items = items[opts.skip:]
		} else {
			items = [][]any{}
		}
	}
	if opts.take >= 0 {
		if opts.take < len(items) {
			items = items[:opts.take]
		}
	}
	res := make([]any, len(items))
	for i, r := range items {
		res[i] = opts.selectFn(r...)
	}
	return res
}
