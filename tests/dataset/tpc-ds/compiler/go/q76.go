//go:build ignore

// Generated by Mochi compiler v0.10.26 on 2025-07-16T01:06:09Z

package main

import (
	"encoding/json"
	"fmt"
	"mochi/runtime/data"
	"reflect"
	"sort"
	"strings"
	"time"

	"golang.org/x/exp/constraints"
)

type Catalog_part struct {
	Channel         string  `json:"channel"`
	Col_name        any     `json:"col_name"`
	D_year          int     `json:"d_year"`
	D_qoy           int     `json:"d_qoy"`
	I_category      string  `json:"i_category"`
	Ext_sales_price float64 `json:"ext_sales_price"`
}

type Catalog_sale struct {
	Cs_bill_customer_sk any     `json:"cs_bill_customer_sk"`
	Cs_item_sk          int     `json:"cs_item_sk"`
	Cs_ext_sales_price  float64 `json:"cs_ext_sales_price"`
	Cs_sold_date_sk     int     `json:"cs_sold_date_sk"`
}

type Date_dim struct {
	D_date_sk int `json:"d_date_sk"`
	D_year    int `json:"d_year"`
	D_qoy     int `json:"d_qoy"`
}

type GKey struct {
	Channel    any `json:"channel"`
	Col_name   any `json:"col_name"`
	D_year     any `json:"d_year"`
	D_qoy      any `json:"d_qoy"`
	I_category any `json:"i_category"`
}

type Item struct {
	I_item_sk  int    `json:"i_item_sk"`
	I_category string `json:"i_category"`
}

type Result struct {
	Channel    any     `json:"channel"`
	Col_name   any     `json:"col_name"`
	D_year     any     `json:"d_year"`
	D_qoy      any     `json:"d_qoy"`
	I_category any     `json:"i_category"`
	Sales_cnt  int     `json:"sales_cnt"`
	Sales_amt  float64 `json:"sales_amt"`
}

type Store_part struct {
	Channel         string  `json:"channel"`
	Col_name        any     `json:"col_name"`
	D_year          int     `json:"d_year"`
	D_qoy           int     `json:"d_qoy"`
	I_category      string  `json:"i_category"`
	Ext_sales_price float64 `json:"ext_sales_price"`
}

type Store_sale struct {
	Ss_customer_sk     any     `json:"ss_customer_sk"`
	Ss_item_sk         int     `json:"ss_item_sk"`
	Ss_ext_sales_price float64 `json:"ss_ext_sales_price"`
	Ss_sold_date_sk    int     `json:"ss_sold_date_sk"`
}

type Web_part struct {
	Channel         string  `json:"channel"`
	Col_name        any     `json:"col_name"`
	D_year          int     `json:"d_year"`
	D_qoy           int     `json:"d_qoy"`
	I_category      string  `json:"i_category"`
	Ext_sales_price float64 `json:"ext_sales_price"`
}

type Web_sale struct {
	Ws_bill_customer_sk any     `json:"ws_bill_customer_sk"`
	Ws_item_sk          int     `json:"ws_item_sk"`
	Ws_ext_sales_price  float64 `json:"ws_ext_sales_price"`
	Ws_sold_date_sk     int     `json:"ws_sold_date_sk"`
}

type v = Result

func expect(cond bool) {
	if !cond {
		panic("expect failed")
	}
}

func formatDuration(d time.Duration) string {
	switch {
	case d < time.Microsecond:
		return fmt.Sprintf("%dns", d.Nanoseconds())
	case d < time.Millisecond:
		return fmt.Sprintf("%.1fµs", float64(d.Microseconds()))
	case d < time.Second:
		return fmt.Sprintf("%.1fms", float64(d.Milliseconds()))
	default:
		return fmt.Sprintf("%.2fs", d.Seconds())
	}
}

func printTestStart(name string) {
	fmt.Printf("   test %-30s ...", name)
}

func printTestPass(d time.Duration) {
	fmt.Printf(" ok (%s)\n", formatDuration(d))
}

func printTestFail(err error, d time.Duration) {
	fmt.Printf(" fail %v (%s)\n", err, formatDuration(d))
}

func test_TPCDS_Q76_simplified() {
	expect(_equal(result, []v{v{
		Channel:    "store",
		Col_name:   nil,
		D_year:     1998,
		D_qoy:      1,
		I_category: "CatA",
		Sales_cnt:  1,
		Sales_amt:  10.0,
	}, v{
		Channel:    "web",
		Col_name:   nil,
		D_year:     1998,
		D_qoy:      1,
		I_category: "CatB",
		Sales_cnt:  1,
		Sales_amt:  15.0,
	}, v{
		Channel:    "catalog",
		Col_name:   nil,
		D_year:     1998,
		D_qoy:      1,
		I_category: "CatC",
		Sales_cnt:  1,
		Sales_amt:  20.0,
	}}))
}

var date_dim []Date_dim
var item []Item
var store_sales []Store_sale
var web_sales []Web_sale
var catalog_sales []Catalog_sale
var store_part []Store_part
var web_part []Web_part
var catalog_part []Catalog_part
var all_rows []any
var result []Result

func main() {
	date_dim = []Date_dim{Date_dim{
		D_date_sk: 1,
		D_year:    1998,
		D_qoy:     1,
	}}
	item = []Item{Item{
		I_item_sk:  1,
		I_category: "CatA",
	}, Item{
		I_item_sk:  2,
		I_category: "CatB",
	}, Item{
		I_item_sk:  3,
		I_category: "CatC",
	}}
	store_sales = []Store_sale{Store_sale{
		Ss_customer_sk:     nil,
		Ss_item_sk:         1,
		Ss_ext_sales_price: 10.0,
		Ss_sold_date_sk:    1,
	}}
	web_sales = []Web_sale{Web_sale{
		Ws_bill_customer_sk: nil,
		Ws_item_sk:          2,
		Ws_ext_sales_price:  15.0,
		Ws_sold_date_sk:     1,
	}}
	catalog_sales = []Catalog_sale{Catalog_sale{
		Cs_bill_customer_sk: nil,
		Cs_item_sk:          3,
		Cs_ext_sales_price:  20.0,
		Cs_sold_date_sk:     1,
	}}
	store_part = func() []Store_part {
		results := []Store_part{}
		for _, ss := range store_sales {
			if _equal(ss.Ss_customer_sk, nil) {
				for _, i := range item {
					if !(i.I_item_sk == ss.Ss_item_sk) {
						continue
					}
					for _, d := range date_dim {
						if !(d.D_date_sk == ss.Ss_sold_date_sk) {
							continue
						}
						results = append(results, Store_part{
							Channel:         "store",
							Col_name:        ss.Ss_customer_sk,
							D_year:          d.D_year,
							D_qoy:           d.D_qoy,
							I_category:      i.I_category,
							Ext_sales_price: ss.Ss_ext_sales_price,
						})
					}
				}
			}
		}
		return results
	}()
	web_part = func() []Web_part {
		results := []Web_part{}
		for _, ws := range web_sales {
			if _equal(ws.Ws_bill_customer_sk, nil) {
				for _, i := range item {
					if !(i.I_item_sk == ws.Ws_item_sk) {
						continue
					}
					for _, d := range date_dim {
						if !(d.D_date_sk == ws.Ws_sold_date_sk) {
							continue
						}
						results = append(results, Web_part{
							Channel:         "web",
							Col_name:        ws.Ws_bill_customer_sk,
							D_year:          d.D_year,
							D_qoy:           d.D_qoy,
							I_category:      i.I_category,
							Ext_sales_price: ws.Ws_ext_sales_price,
						})
					}
				}
			}
		}
		return results
	}()
	catalog_part = func() []Catalog_part {
		results := []Catalog_part{}
		for _, cs := range catalog_sales {
			if _equal(cs.Cs_bill_customer_sk, nil) {
				for _, i := range item {
					if !(i.I_item_sk == cs.Cs_item_sk) {
						continue
					}
					for _, d := range date_dim {
						if !(d.D_date_sk == cs.Cs_sold_date_sk) {
							continue
						}
						results = append(results, Catalog_part{
							Channel:         "catalog",
							Col_name:        cs.Cs_bill_customer_sk,
							D_year:          d.D_year,
							D_qoy:           d.D_qoy,
							I_category:      i.I_category,
							Ext_sales_price: cs.Cs_ext_sales_price,
						})
					}
				}
			}
		}
		return results
	}()
	all_rows = _concat[any](_concat[any](_toAnySlice(store_part), _toAnySlice(web_part)), _toAnySlice(catalog_part))
	result = func() []Result {
		groups := map[string]*data.Group{}
		order := []string{}
		for _, r := range all_rows {
			key := GKey{
				Channel:    _getField(r, "channel"),
				Col_name:   _getField(r, "col_name"),
				D_year:     _getField(r, "d_year"),
				D_qoy:      _getField(r, "d_qoy"),
				I_category: _getField(r, "i_category"),
			}
			ks := fmt.Sprint(key)
			g, ok := groups[ks]
			if !ok {
				g = &data.Group{Key: key}
				groups[ks] = g
				order = append(order, ks)
			}
			g.Items = append(g.Items, r)
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
			pairs[idx] = pair{item: it, key: g.Key.(GKey).Channel}
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
				Channel:    g.Key.(GKey).Channel,
				Col_name:   g.Key.(GKey).Col_name,
				D_year:     g.Key.(GKey).D_year,
				D_qoy:      g.Key.(GKey).D_qoy,
				I_category: g.Key.(GKey).I_category,
				Sales_cnt:  len(g.Items),
				Sales_amt: _sum(func() []any {
					results := []any{}
					for _, xRaw := range g.Items {
						x := xRaw.(any)
						results = append(results, _getField(_getField(x, "r"), "ext_sales_price"))
					}
					return results
				}()),
			})
		}
		return results
	}()
	func() { b, _ := json.Marshal(result); fmt.Println(string(b)) }()
	test_TPCDS_Q76_simplified()
}

func _concat[T any](a, b []T) []T {
	res := make([]T, 0, len(a)+len(b))
	res = append(res, a...)
	res = append(res, b...)
	return res
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

func _equal(a, b any) bool {
	av := reflect.ValueOf(a)
	bv := reflect.ValueOf(b)
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

func _sum[T constraints.Integer | constraints.Float](v []T) float64 {
	var sum float64
	for _, n := range v {
		sum += float64(n)
	}
	return sum
}

func _toAnySlice[T any](s []T) []any {
	out := make([]any, len(s))
	for i, v := range s {
		out[i] = v
	}
	return out
}
