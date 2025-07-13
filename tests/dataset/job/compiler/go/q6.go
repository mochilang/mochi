//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
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

func test_Q6_finds_marvel_movie_with_Robert_Downey() {
	expect(_equal(result, []map[string]string{map[string]string{
		"movie_keyword": "marvel-cinematic-universe",
		"actor_name":    "Downey Robert Jr.",
		"marvel_movie":  "Iron Man 3",
	}}))
}

type Cast_infoItem struct {
	Movie_id  int `json:"movie_id"`
	Person_id int `json:"person_id"`
}

var cast_info []Cast_infoItem

type KeywordItem struct {
	Id      int    `json:"id"`
	Keyword string `json:"keyword"`
}

var keyword []KeywordItem

type Movie_keywordItem struct {
	Movie_id   int `json:"movie_id"`
	Keyword_id int `json:"keyword_id"`
}

var movie_keyword []Movie_keywordItem

type NameItem struct {
	Id   int    `json:"id"`
	Name string `json:"name"`
}

var name []NameItem

type TitleItem struct {
	Id              int    `json:"id"`
	Title           string `json:"title"`
	Production_year int    `json:"production_year"`
}

var title []TitleItem
var result []map[string]string

func main() {
	failures := 0
	cast_info = _cast[[]Cast_infoItem]([]Cast_infoItem{Cast_infoItem{
		Movie_id:  1,
		Person_id: 101,
	}, Cast_infoItem{
		Movie_id:  2,
		Person_id: 102,
	}})
	keyword = _cast[[]KeywordItem]([]KeywordItem{KeywordItem{
		Id:      100,
		Keyword: "marvel-cinematic-universe",
	}, KeywordItem{
		Id:      200,
		Keyword: "other",
	}})
	movie_keyword = _cast[[]Movie_keywordItem]([]Movie_keywordItem{Movie_keywordItem{
		Movie_id:   1,
		Keyword_id: 100,
	}, Movie_keywordItem{
		Movie_id:   2,
		Keyword_id: 200,
	}})
	name = _cast[[]NameItem]([]NameItem{NameItem{
		Id:   101,
		Name: "Downey Robert Jr.",
	}, NameItem{
		Id:   102,
		Name: "Chris Evans",
	}})
	title = _cast[[]TitleItem]([]TitleItem{TitleItem{
		Id:              1,
		Title:           "Iron Man 3",
		Production_year: 2013,
	}, TitleItem{
		Id:              2,
		Title:           "Old Movie",
		Production_year: 2000,
	}})
	result = func() []map[string]string {
		_res := []map[string]string{}
		for _, ci := range cast_info {
			for _, mk := range movie_keyword {
				if !(ci.Movie_id == mk.Movie_id) {
					continue
				}
				for _, k := range keyword {
					if !(mk.Keyword_id == k.Id) {
						continue
					}
					for _, n := range name {
						if !(ci.Person_id == n.Id) {
							continue
						}
						for _, t := range title {
							if !(ci.Movie_id == t.Id) {
								continue
							}
							if (((k.Keyword == "marvel-cinematic-universe") && strings.Contains(n.Name, "Downey")) && strings.Contains(n.Name, "Robert")) && (t.Production_year > 2010) {
								if (((k.Keyword == "marvel-cinematic-universe") && strings.Contains(n.Name, "Downey")) && strings.Contains(n.Name, "Robert")) && (t.Production_year > 2010) {
									_res = append(_res, map[string]string{
										"movie_keyword": k.Keyword,
										"actor_name":    n.Name,
										"marvel_movie":  t.Title,
									})
								}
							}
						}
					}
				}
			}
		}
		return _res
	}()
	func() { b, _ := json.Marshal(result); fmt.Println(string(b)) }()
	{
		printTestStart("Q6 finds marvel movie with Robert Downey")
		start := time.Now()
		var failed error
		func() {
			defer func() {
				if r := recover(); r != nil {
					failed = fmt.Errorf("%v", r)
				}
			}()
			test_Q6_finds_marvel_movie_with_Robert_Downey()
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
