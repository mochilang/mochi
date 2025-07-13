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

func test_Q10_finds_uncredited_voice_actor_in_Russian_movie() {
	expect(_equal(result, []map[string]string{map[string]string{"uncredited_voiced_character": "Ivan", "russian_movie": "Vodka Dreams"}}))
}

type Char_nameItem struct {
	Id   int    `json:"id"`
	Name string `json:"name"`
}

var char_name []Char_nameItem

type Cast_infoItem struct {
	Movie_id       int    `json:"movie_id"`
	Person_role_id int    `json:"person_role_id"`
	Role_id        int    `json:"role_id"`
	Note           string `json:"note"`
}

var cast_info []Cast_infoItem

type Company_nameItem struct {
	Id           int    `json:"id"`
	Country_code string `json:"country_code"`
}

var company_name []Company_nameItem

type Company_typeItem struct {
	Id int `json:"id"`
}

var company_type []Company_typeItem

type Movie_companiesItem struct {
	Movie_id        int `json:"movie_id"`
	Company_id      int `json:"company_id"`
	Company_type_id int `json:"company_type_id"`
}

var movie_companies []Movie_companiesItem

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
	Uncredited_voiced_character any `json:"uncredited_voiced_character"`
	Russian_movie               any `json:"russian_movie"`
}

var result []ResultItem

func main() {
	failures := 0
	char_name = _cast[[]Char_nameItem]([]Char_nameItem{Char_nameItem{
		Id:   1,
		Name: "Ivan",
	}, Char_nameItem{
		Id:   2,
		Name: "Alex",
	}})
	cast_info = _cast[[]Cast_infoItem]([]Cast_infoItem{Cast_infoItem{
		Movie_id:       10,
		Person_role_id: 1,
		Role_id:        1,
		Note:           "Soldier (voice) (uncredited)",
	}, Cast_infoItem{
		Movie_id:       11,
		Person_role_id: 2,
		Role_id:        1,
		Note:           "(voice)",
	}})
	company_name = _cast[[]Company_nameItem]([]Company_nameItem{Company_nameItem{
		Id:           1,
		Country_code: "[ru]",
	}, Company_nameItem{
		Id:           2,
		Country_code: "[us]",
	}})
	company_type = _cast[[]Company_typeItem]([]Company_typeItem{Company_typeItem{Id: 1}, Company_typeItem{Id: 2}})
	movie_companies = _cast[[]Movie_companiesItem]([]Movie_companiesItem{Movie_companiesItem{
		Movie_id:        10,
		Company_id:      1,
		Company_type_id: 1,
	}, Movie_companiesItem{
		Movie_id:        11,
		Company_id:      2,
		Company_type_id: 1,
	}})
	role_type = _cast[[]Role_typeItem]([]Role_typeItem{Role_typeItem{
		Id:   1,
		Role: "actor",
	}, Role_typeItem{
		Id:   2,
		Role: "director",
	}})
	title = _cast[[]TitleItem]([]TitleItem{TitleItem{
		Id:              10,
		Title:           "Vodka Dreams",
		Production_year: 2006,
	}, TitleItem{
		Id:              11,
		Title:           "Other Film",
		Production_year: 2004,
	}})
	matches = func() []map[string]string {
		_res := []map[string]string{}
		for _, chn := range char_name {
			for _, ci := range cast_info {
				if !(chn.Id == ci.Person_role_id) {
					continue
				}
				for _, rt := range role_type {
					if !(rt.Id == ci.Role_id) {
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
								if (((strings.Contains(ci.Note, "(voice)") && strings.Contains(ci.Note, "(uncredited)")) && (cn.Country_code == "[ru]")) && (rt.Role == "actor")) && (t.Production_year > 2005) {
									for _, ct := range company_type {
										if !(ct.Id == mc.Company_type_id) {
											continue
										}
										_res = append(_res, map[string]string{"character": chn.Name, "movie": t.Title})
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
		Uncredited_voiced_character: _min(func() []string {
			_res := []string{}
			for _, x := range matches {
				_res = append(_res, x["character"])
			}
			return _res
		}()),
		Russian_movie: _min(func() []string {
			_res := []string{}
			for _, x := range matches {
				_res = append(_res, x["movie"])
			}
			return _res
		}()),
	}})
	func() { b, _ := json.Marshal(result); fmt.Println(string(b)) }()
	{
		printTestStart("Q10 finds uncredited voice actor in Russian movie")
		start := time.Now()
		var failed error
		func() {
			defer func() {
				if r := recover(); r != nil {
					failed = fmt.Errorf("%v", r)
				}
			}()
			test_Q10_finds_uncredited_voice_actor_in_Russian_movie()
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
