package data

import (
	"fmt"
	"sort"
)

// Group represents a group of items with a common key.
type Group struct {
	Key   any
	Items []any
}

// QueryOptions defines callbacks for evaluating a query over a list of items.
type QueryOptions struct {
	Where       func(item any) (bool, error)
	GroupBy     func(item any) (any, error)
	Select      func(item any) (any, error)
	SelectGroup func(g *Group) (any, error)
	SortKey     func(item any) (any, error)
	Skip        *int
	Take        *int
}

// Query executes a query over src using the provided options.
// It implements filtering, grouping, sorting, skipping and taking
// semantics used by Mochi's "from" expression.
func Query(src []any, opt QueryOptions) ([]any, error) {
	items := make([]any, 0, len(src))
	for _, it := range src {
		if opt.Where != nil {
			keep, err := opt.Where(it)
			if err != nil {
				return nil, err
			}
			if !keep {
				continue
			}
		}
		items = append(items, it)
	}

	if opt.GroupBy != nil {
		groups := map[string]*Group{}
		order := []string{}
		for _, item := range items {
			key, err := opt.GroupBy(item)
			if err != nil {
				return nil, err
			}
			ks := fmt.Sprint(key)
			g, ok := groups[ks]
			if !ok {
				g = &Group{Key: key}
				groups[ks] = g
				order = append(order, ks)
			}
			g.Items = append(g.Items, item)
		}
		results := make([]any, 0, len(groups))
		for _, ks := range order {
			g := groups[ks]
			if opt.SelectGroup != nil {
				val, err := opt.SelectGroup(g)
				if err != nil {
					return nil, err
				}
				results = append(results, val)
			} else {
				results = append(results, g)
			}
		}
		return results, nil
	}

	if opt.SortKey != nil {
		type pair struct {
			item any
			key  any
		}
		pairs := make([]pair, len(items))
		for idx, it := range items {
			key, err := opt.SortKey(it)
			if err != nil {
				return nil, err
			}
			pairs[idx] = pair{it, key}
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
	}

	if opt.Skip != nil {
		n := *opt.Skip
		if n < len(items) {
			items = items[n:]
		} else {
			items = []any{}
		}
	}

	if opt.Take != nil {
		n := *opt.Take
		if n < len(items) {
			items = items[:n]
		}
	}

	results := make([]any, 0, len(items))
	for _, item := range items {
		if opt.Select != nil {
			val, err := opt.Select(item)
			if err != nil {
				return nil, err
			}
			results = append(results, val)
		} else {
			results = append(results, item)
		}
	}
	return results, nil
}
