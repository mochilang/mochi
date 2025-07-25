//go:build ignore

// Generated by Mochi compiler v0.10.26 on 2025-07-16T01:04:38Z

package main

import (
	"encoding/json"
	"fmt"
	"mochi/runtime/data"
	"reflect"
	"slices"
	"time"

	"golang.org/x/exp/constraints"
)

type CatalogSale struct {
	Cs_item_sk      int     `json:"cs_item_sk"`
	Cs_list_price   float64 `json:"cs_list_price"`
	Cs_quantity     int     `json:"cs_quantity"`
	Cs_sold_date_sk int     `json:"cs_sold_date_sk"`
}

type Catalog_sale struct {
	Cs_item_sk      int     `json:"cs_item_sk"`
	Cs_list_price   float64 `json:"cs_list_price"`
	Cs_quantity     int     `json:"cs_quantity"`
	Cs_sold_date_sk int     `json:"cs_sold_date_sk"`
}

type Cross_item struct {
	Ss_item_sk int `json:"ss_item_sk"`
}

type DateDim struct {
	D_date_sk int `json:"d_date_sk"`
	D_year    int `json:"d_year"`
	D_moy     int `json:"d_moy"`
}

type Date_dim struct {
	D_date_sk int `json:"d_date_sk"`
	D_year    int `json:"d_year"`
	D_moy     int `json:"d_moy"`
}

type GKey struct {
	Brand_id    int `json:"brand_id"`
	Class_id    int `json:"class_id"`
	Category_id int `json:"category_id"`
}

type Item struct {
	I_item_sk     int `json:"i_item_sk"`
	I_brand_id    int `json:"i_brand_id"`
	I_class_id    int `json:"i_class_id"`
	I_category_id int `json:"i_category_id"`
}

type Result struct {
	Channel       string  `json:"channel"`
	I_brand_id    int     `json:"i_brand_id"`
	I_class_id    int     `json:"i_class_id"`
	I_category_id int     `json:"i_category_id"`
	Sales         float64 `json:"sales"`
	Number_sales  int     `json:"number_sales"`
}

type StoreSale struct {
	Ss_item_sk      int     `json:"ss_item_sk"`
	Ss_list_price   float64 `json:"ss_list_price"`
	Ss_quantity     int     `json:"ss_quantity"`
	Ss_sold_date_sk int     `json:"ss_sold_date_sk"`
}

type Store_filtered struct {
	Channel      string  `json:"channel"`
	Sales        float64 `json:"sales"`
	Number_sales int     `json:"number_sales"`
}

type Store_sale struct {
	Ss_item_sk      int     `json:"ss_item_sk"`
	Ss_list_price   float64 `json:"ss_list_price"`
	Ss_quantity     int     `json:"ss_quantity"`
	Ss_sold_date_sk int     `json:"ss_sold_date_sk"`
}

type WebSale struct {
	Ws_item_sk      int     `json:"ws_item_sk"`
	Ws_list_price   float64 `json:"ws_list_price"`
	Ws_quantity     int     `json:"ws_quantity"`
	Ws_sold_date_sk int     `json:"ws_sold_date_sk"`
}

type Web_sale struct {
	Ws_item_sk      int     `json:"ws_item_sk"`
	Ws_list_price   float64 `json:"ws_list_price"`
	Ws_quantity     int     `json:"ws_quantity"`
	Ws_sold_date_sk int     `json:"ws_sold_date_sk"`
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

func test_TPCDS_Q14_cross_channel() {
	expect(_equal(result, []v{v{
		Channel:       "store",
		I_brand_id:    1,
		I_class_id:    1,
		I_category_id: 1,
		Sales:         60.0,
		Number_sales:  1,
	}}))
}

var store_sales []Store_sale
var catalog_sales []Catalog_sale
var web_sales []Web_sale
var item []Item
var date_dim []Date_dim
var cross_items []Cross_item
var avg_sales float64
var store_filtered []Store_filtered
var result []Result

func main() {
	store_sales = []Store_sale{Store_sale{
		Ss_item_sk:      1,
		Ss_list_price:   10.0,
		Ss_quantity:     2,
		Ss_sold_date_sk: 1,
	}, Store_sale{
		Ss_item_sk:      1,
		Ss_list_price:   20.0,
		Ss_quantity:     3,
		Ss_sold_date_sk: 2,
	}}
	catalog_sales = []Catalog_sale{Catalog_sale{
		Cs_item_sk:      1,
		Cs_list_price:   10.0,
		Cs_quantity:     2,
		Cs_sold_date_sk: 1,
	}}
	web_sales = []Web_sale{Web_sale{
		Ws_item_sk:      1,
		Ws_list_price:   30.0,
		Ws_quantity:     1,
		Ws_sold_date_sk: 1,
	}}
	item = []Item{Item{
		I_item_sk:     1,
		I_brand_id:    1,
		I_class_id:    1,
		I_category_id: 1,
	}}
	date_dim = []Date_dim{Date_dim{
		D_date_sk: 1,
		D_year:    2000,
		D_moy:     12,
	}, Date_dim{
		D_date_sk: 2,
		D_year:    2002,
		D_moy:     11,
	}}
	cross_items = []Cross_item{Cross_item{Ss_item_sk: 1}}
	avg_sales = _avgOrdered[float64]([]float64{20.0, 20.0, 30.0})
	store_filtered = func() []Store_filtered {
		groups := map[string]*data.Group{}
		order := []string{}
		for _, ss := range store_sales {
			for _, d := range date_dim {
				if !(((ss.Ss_sold_date_sk == d.D_date_sk) && (d.D_year == 2002)) && (d.D_moy == 11)) {
					continue
				}
				if slices.Contains((func() []int {
					results := []int{}
					for _, ci := range cross_items {
						results = append(results, ci.Ss_item_sk)
					}
					return results
				}()), ss.Ss_item_sk) {
					key := GKey{
						Brand_id:    1,
						Class_id:    1,
						Category_id: 1,
					}
					ks := fmt.Sprint(key)
					g, ok := groups[ks]
					if !ok {
						g = &data.Group{Key: key}
						groups[ks] = g
						order = append(order, ks)
					}
					g.Items = append(g.Items, ss)
				}
			}
		}
		items := []*data.Group{}
		for _, ks := range order {
			items = append(items, groups[ks])
		}
		results := []Store_filtered{}
		for _, g := range items {
			results = append(results, Store_filtered{
				Channel: "store",
				Sales: _sumOrdered[float64](func() []float64 {
					results := []float64{}
					for _, xRaw := range g.Items {
						x := xRaw.(Store_sale)
						results = append(results, (float64(x.Ss_quantity) * x.Ss_list_price))
					}
					return results
				}()),
				Number_sales: len(func() []Store_sale {
					results := []Store_sale{}
					for _, vRaw := range g.Items {
						v := vRaw.(Store_sale)
						results = append(results, v)
					}
					return results
				}()),
			})
		}
		return results
	}()
	result = func() []Result {
		results := []Result{}
		for _, r := range store_filtered {
			if r.Sales > avg_sales {
				if r.Sales > avg_sales {
					results = append(results, Result{
						Channel:       r.Channel,
						I_brand_id:    1,
						I_class_id:    1,
						I_category_id: 1,
						Sales:         r.Sales,
						Number_sales:  r.Number_sales,
					})
				}
			}
		}
		return results
	}()
	func() { b, _ := json.Marshal(result); fmt.Println(string(b)) }()
	test_TPCDS_Q14_cross_channel()
}

func _avgOrdered[T constraints.Integer | constraints.Float](s []T) float64 {
	if len(s) == 0 {
		return 0
	}
	var sum float64
	for _, v := range s {
		sum += float64(v)
	}
	return sum / float64(len(s))
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

func _sumOrdered[T constraints.Integer | constraints.Float](s []T) float64 {
	var sum float64
	for _, v := range s {
		sum += float64(v)
	}
	return sum
}
