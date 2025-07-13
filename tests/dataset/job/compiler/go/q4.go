//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
	"mochi/runtime/data"
	"reflect"
	"strings"
	"time"
)

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

func test_Q4_returns_minimum_rating_and_title_for_sequels() {
	expect(_equal(result, []map[string]string{map[string]string{"rating": "6.2", "movie_title": "Alpha Movie"}}))
}

type Info_typeItem struct {
	Id   int    `json:"id"`
	Info string `json:"info"`
}

var info_type []Info_typeItem

type KeywordItem struct {
	Id      int    `json:"id"`
	Keyword string `json:"keyword"`
}

var keyword []KeywordItem

type TitleItem struct {
	Id              int    `json:"id"`
	Title           string `json:"title"`
	Production_year int    `json:"production_year"`
}

var title []TitleItem

type Movie_keywordItem struct {
	Movie_id   int `json:"movie_id"`
	Keyword_id int `json:"keyword_id"`
}

var movie_keyword []Movie_keywordItem

type Movie_info_idxItem struct {
	Movie_id     int    `json:"movie_id"`
	Info_type_id int    `json:"info_type_id"`
	Info         string `json:"info"`
}

var movie_info_idx []Movie_info_idxItem
var rows []map[string]string

type ResultItem struct {
	Rating      any `json:"rating"`
	Movie_title any `json:"movie_title"`
}

var result []ResultItem

func main() {
	failures := 0
	info_type = _cast[[]Info_typeItem]([]Info_typeItem{Info_typeItem{
		Id:   1,
		Info: "rating",
	}, Info_typeItem{
		Id:   2,
		Info: "other",
	}})
	keyword = _cast[[]KeywordItem]([]KeywordItem{KeywordItem{
		Id:      1,
		Keyword: "great sequel",
	}, KeywordItem{
		Id:      2,
		Keyword: "prequel",
	}})
	title = _cast[[]TitleItem]([]TitleItem{TitleItem{
		Id:              10,
		Title:           "Alpha Movie",
		Production_year: 2006,
	}, TitleItem{
		Id:              20,
		Title:           "Beta Film",
		Production_year: 2007,
	}, TitleItem{
		Id:              30,
		Title:           "Old Film",
		Production_year: 2004,
	}})
	movie_keyword = _cast[[]Movie_keywordItem]([]Movie_keywordItem{Movie_keywordItem{
		Movie_id:   10,
		Keyword_id: 1,
	}, Movie_keywordItem{
		Movie_id:   20,
		Keyword_id: 1,
	}, Movie_keywordItem{
		Movie_id:   30,
		Keyword_id: 1,
	}})
	movie_info_idx = _cast[[]Movie_info_idxItem]([]Movie_info_idxItem{Movie_info_idxItem{
		Movie_id:     10,
		Info_type_id: 1,
		Info:         "6.2",
	}, Movie_info_idxItem{
		Movie_id:     20,
		Info_type_id: 1,
		Info:         "7.8",
	}, Movie_info_idxItem{
		Movie_id:     30,
		Info_type_id: 1,
		Info:         "4.5",
	}})
	rows = func() []map[string]string {
		_res := []map[string]string{}
		for _, it := range info_type {
			for _, mi := range movie_info_idx {
				if !(it.Id == mi.Info_type_id) {
					continue
				}
				for _, t := range title {
					if !(t.Id == mi.Movie_id) {
						continue
					}
					for _, mk := range movie_keyword {
						if !(mk.Movie_id == t.Id) {
							continue
						}
						for _, k := range keyword {
							if !(k.Id == mk.Keyword_id) {
								continue
							}
							if ((((it.Info == "rating") && strings.Contains(k.Keyword, "sequel")) && (mi.Info > "5.0")) && (t.Production_year > 2005)) && (mk.Movie_id == mi.Movie_id) {
								if ((((it.Info == "rating") && strings.Contains(k.Keyword, "sequel")) && (mi.Info > "5.0")) && (t.Production_year > 2005)) && (mk.Movie_id == mi.Movie_id) {
									_res = append(_res, map[string]string{"rating": mi.Info, "title": t.Title})
								}
							}
						}
					}
				}
			}
		}
		return _res
	}()
	result = _cast[[]ResultItem]([]ResultItem{ResultItem{
		Rating: _min(func() []string {
			_res := []string{}
			for _, r := range rows {
				_res = append(_res, r["rating"])
			}
			return _res
		}()),
		Movie_title: _min(func() []string {
			_res := []string{}
			for _, r := range rows {
				_res = append(_res, r["title"])
			}
			return _res
		}()),
	}})
	func() { b, _ := json.Marshal(result); fmt.Println(string(b)) }()
	{
		printTestStart("Q4 returns minimum rating and title for sequels")
		start := time.Now()
		var failed error
		func() {
			defer func() {
				if r := recover(); r != nil {
					failed = fmt.Errorf("%v", r)
				}
			}()
			test_Q4_returns_minimum_rating_and_title_for_sequels()
		}()
		if failed != nil {
			failures++
			printTestFail(failed, time.Since(start))
		} else {
			printTestPass(time.Since(start))
		}
	}
	if failures > 0 {
		fmt.Printf("\n[FAIL] %d test(s) failed.\n", failures)
	}
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

func _min(v any) any {
	if g, ok := v.(*data.Group); ok {
		v = g.Items
	}
	switch s := v.(type) {
	case []int:
		if len(s) == 0 {
			return 0
		}
		m := s[0]
		for _, n := range s[1:] {
			if n < m {
				m = n
			}
		}
		return m
	case []float64:
		if len(s) == 0 {
			return 0.0
		}
		m := s[0]
		for _, n := range s[1:] {
			if n < m {
				m = n
			}
		}
		return m
	case []string:
		if len(s) == 0 {
			return ""
		}
		m := s[0]
		for _, n := range s[1:] {
			if n < m {
				m = n
			}
		}
		return m
	case []any:
		if len(s) == 0 {
			return 0
		}
		switch s[0].(type) {
		case string:
			m := s[0].(string)
			for _, it := range s[1:] {
				v := it.(string)
				if v < m {
					m = v
				}
			}
			return m
		case int, int64, float64:
			var m float64
			var isFloat bool
			switch n := s[0].(type) {
			case int:
				m = float64(n)
			case int64:
				m = float64(n)
			case float64:
				m = n
				isFloat = true
			}
			for _, it := range s[1:] {
				switch v := it.(type) {
				case int:
					if float64(v) < m {
						m = float64(v)
					}
				case int64:
					if float64(v) < m {
						m = float64(v)
					}
				case float64:
					if v < m {
						m = v
					}
					isFloat = true
				}
			}
			if isFloat {
				return m
			}
			return int(m)
		default:
			panic("min() expects numbers or strings")
		}
	default:
		rv := reflect.ValueOf(v)
		if rv.Kind() == reflect.Slice {
			if rv.Len() == 0 {
				return 0
			}
			m := rv.Index(0).Interface()
			switch m.(type) {
			case int, int64, float64:
				items := make([]any, rv.Len())
				for i := 0; i < rv.Len(); i++ {
					items[i] = rv.Index(i).Interface()
				}
				return _min(items)
			}
		}
		panic("min() expects list or group")
	}
}
