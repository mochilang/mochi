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

func test_Q9_selects_minimal_alternative_name__character_and_movie() {
	expect(_equal(result, []map[string]string{map[string]string{
		"alternative_name": "A. N. G.",
		"character_name":   "Angel",
		"movie":            "Famous Film",
	}}))
}

type Aka_nameItem struct {
	Person_id int    `json:"person_id"`
	Name      string `json:"name"`
}

var aka_name []Aka_nameItem

type Char_nameItem struct {
	Id   int    `json:"id"`
	Name string `json:"name"`
}

var char_name []Char_nameItem

type Cast_infoItem struct {
	Person_id      int    `json:"person_id"`
	Person_role_id int    `json:"person_role_id"`
	Movie_id       int    `json:"movie_id"`
	Role_id        int    `json:"role_id"`
	Note           string `json:"note"`
}

var cast_info []Cast_infoItem

type Company_nameItem struct {
	Id           int    `json:"id"`
	Country_code string `json:"country_code"`
}

var company_name []Company_nameItem

type Movie_companiesItem struct {
	Movie_id   int    `json:"movie_id"`
	Company_id int    `json:"company_id"`
	Note       string `json:"note"`
}

var movie_companies []Movie_companiesItem

type NameItem struct {
	Id     int    `json:"id"`
	Name   string `json:"name"`
	Gender string `json:"gender"`
}

var name []NameItem

type Role_typeItem struct {
	Id   int    `json:"id"`
	Role string `json:"role"`
}

var role_type []Role_typeItem

type TitleItem struct {
	Id              int    `json:"id"`
	Title           string `json:"title"`
	Production_year int    `json:"production_year"`
}

var title []TitleItem
var matches []map[string]string

type ResultItem struct {
	Alternative_name any `json:"alternative_name"`
	Character_name   any `json:"character_name"`
	Movie            any `json:"movie"`
}

var result []ResultItem

func main() {
	failures := 0
	aka_name = _cast[[]Aka_nameItem]([]Aka_nameItem{Aka_nameItem{
		Person_id: 1,
		Name:      "A. N. G.",
	}, Aka_nameItem{
		Person_id: 2,
		Name:      "J. D.",
	}})
	char_name = _cast[[]Char_nameItem]([]Char_nameItem{Char_nameItem{
		Id:   10,
		Name: "Angel",
	}, Char_nameItem{
		Id:   20,
		Name: "Devil",
	}})
	cast_info = _cast[[]Cast_infoItem]([]Cast_infoItem{Cast_infoItem{
		Person_id:      1,
		Person_role_id: 10,
		Movie_id:       100,
		Role_id:        1000,
		Note:           "(voice)",
	}, Cast_infoItem{
		Person_id:      2,
		Person_role_id: 20,
		Movie_id:       200,
		Role_id:        1000,
		Note:           "(voice)",
	}})
	company_name = _cast[[]Company_nameItem]([]Company_nameItem{Company_nameItem{
		Id:           100,
		Country_code: "[us]",
	}, Company_nameItem{
		Id:           200,
		Country_code: "[gb]",
	}})
	movie_companies = _cast[[]Movie_companiesItem]([]Movie_companiesItem{Movie_companiesItem{
		Movie_id:   100,
		Company_id: 100,
		Note:       "ACME Studios (USA)",
	}, Movie_companiesItem{
		Movie_id:   200,
		Company_id: 200,
		Note:       "Maple Films",
	}})
	name = _cast[[]NameItem]([]NameItem{NameItem{
		Id:     1,
		Name:   "Angela Smith",
		Gender: "f",
	}, NameItem{
		Id:     2,
		Name:   "John Doe",
		Gender: "m",
	}})
	role_type = _cast[[]Role_typeItem]([]Role_typeItem{Role_typeItem{
		Id:   1000,
		Role: "actress",
	}, Role_typeItem{
		Id:   2000,
		Role: "actor",
	}})
	title = _cast[[]TitleItem]([]TitleItem{TitleItem{
		Id:              100,
		Title:           "Famous Film",
		Production_year: 2010,
	}, TitleItem{
		Id:              200,
		Title:           "Old Movie",
		Production_year: 1999,
	}})
	matches = func() []map[string]string {
		_res := []map[string]string{}
		for _, an := range aka_name {
			for _, n := range name {
				if !(an.Person_id == n.Id) {
					continue
				}
				for _, ci := range cast_info {
					if !(ci.Person_id == n.Id) {
						continue
					}
					for _, chn := range char_name {
						if !(chn.Id == ci.Person_role_id) {
							continue
						}
						for _, t := range title {
							if !(t.Id == ci.Movie_id) {
								continue
							}
							for _, mc := range movie_companies {
								if !(mc.Movie_id == t.Id) {
									continue
								}
								for _, cn := range company_name {
									if !(cn.Id == mc.Company_id) {
										continue
									}
									for _, rt := range role_type {
										if !(rt.Id == ci.Role_id) {
											continue
										}
										if (((((((_contains[string]([]string{
											"(voice)",
											"(voice: Japanese version)",
											"(voice) (uncredited)",
											"(voice: English version)",
										}, ci.Note)) && (cn.Country_code == "[us]")) && (strings.Contains(mc.Note, "(USA)") || strings.Contains(mc.Note, "(worldwide)"))) && (n.Gender == "f")) && strings.Contains(n.Name, "Ang")) && (rt.Role == "actress")) && (t.Production_year >= 2005)) && (t.Production_year <= 2015) {
											if (((((((_contains[string]([]string{
												"(voice)",
												"(voice: Japanese version)",
												"(voice) (uncredited)",
												"(voice: English version)",
											}, ci.Note)) && (cn.Country_code == "[us]")) && (strings.Contains(mc.Note, "(USA)") || strings.Contains(mc.Note, "(worldwide)"))) && (n.Gender == "f")) && strings.Contains(n.Name, "Ang")) && (rt.Role == "actress")) && (t.Production_year >= 2005)) && (t.Production_year <= 2015) {
												_res = append(_res, map[string]string{
													"alt":       an.Name,
													"character": chn.Name,
													"movie":     t.Title,
												})
											}
										}
									}
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
		Alternative_name: _min(func() []string {
			_res := []string{}
			for _, x := range matches {
				_res = append(_res, x["alt"])
			}
			return _res
		}()),
		Character_name: _min(func() []string {
			_res := []string{}
			for _, x := range matches {
				_res = append(_res, x["character"])
			}
			return _res
		}()),
		Movie: _min(func() []string {
			_res := []string{}
			for _, x := range matches {
				_res = append(_res, x["movie"])
			}
			return _res
		}()),
	}})
	func() { b, _ := json.Marshal(result); fmt.Println(string(b)) }()
	{
		printTestStart("Q9 selects minimal alternative name, character and movie")
		start := time.Now()
		var failed error
		func() {
			defer func() {
				if r := recover(); r != nil {
					failed = fmt.Errorf("%v", r)
				}
			}()
			test_Q9_selects_minimal_alternative_name__character_and_movie()
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

func _contains[T comparable](s []T, v T) bool {
	for _, x := range s {
		if x == v {
			return true
		}
	}
	return false
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
