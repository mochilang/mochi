//go:build ignore

// Generated by Mochi compiler v0.10.25 on 2025-07-15T03:06:44Z

package main

import (
	"encoding/json"
	"fmt"
	"reflect"
	"strings"
	"time"
)

type Cast_info struct {
	Movie_id  int `json:"movie_id"`
	Person_id int `json:"person_id"`
}

type Keyword struct {
	ID      int    `json:"id"`
	Keyword string `json:"keyword"`
}

type Movie_keyword struct {
	Movie_id   int `json:"movie_id"`
	Keyword_id int `json:"keyword_id"`
}

type Name struct {
	ID   int    `json:"id"`
	Name string `json:"name"`
}

type Result struct {
	Movie_keyword string `json:"movie_keyword"`
	Actor_name    string `json:"actor_name"`
	Marvel_movie  string `json:"marvel_movie"`
}

type Title struct {
	ID              int    `json:"id"`
	Title           string `json:"title"`
	Production_year int    `json:"production_year"`
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

func test_Q6_finds_marvel_movie_with_Robert_Downey() {
	expect(_equal(result, []v{v{
		Movie_keyword: "marvel-cinematic-universe",
		Actor_name:    "Downey Robert Jr.",
		Marvel_movie:  "Iron Man 3",
	}}))
}

var cast_info []Cast_info
var keyword []Keyword
var movie_keyword []Movie_keyword
var name []Name
var title []Title
var result []Result

func main() {
	cast_info = []Cast_info{Cast_info{
		Movie_id:  1,
		Person_id: 101,
	}, Cast_info{
		Movie_id:  2,
		Person_id: 102,
	}}
	keyword = []Keyword{Keyword{
		ID:      100,
		Keyword: "marvel-cinematic-universe",
	}, Keyword{
		ID:      200,
		Keyword: "other",
	}}
	movie_keyword = []Movie_keyword{Movie_keyword{
		Movie_id:   1,
		Keyword_id: 100,
	}, Movie_keyword{
		Movie_id:   2,
		Keyword_id: 200,
	}}
	name = []Name{Name{
		ID:   101,
		Name: "Downey Robert Jr.",
	}, Name{
		ID:   102,
		Name: "Chris Evans",
	}}
	title = []Title{Title{
		ID:              1,
		Title:           "Iron Man 3",
		Production_year: 2013,
	}, Title{
		ID:              2,
		Title:           "Old Movie",
		Production_year: 2000,
	}}
	result = func() []Result {
		results := []Result{}
		for _, ciRaw := range cast_info {
			ci := ciRaw.(Cast_info)
			for _, mk := range movie_keyword {
				if !(ci.Movie_id == mk.Movie_id) {
					continue
				}
				for _, k := range keyword {
					if !(mk.Keyword_id == k.ID) {
						continue
					}
					for _, n := range name {
						if !(ci.Person_id == n.ID) {
							continue
						}
						for _, t := range title {
							if !(ci.Movie_id == t.ID) {
								continue
							}
							if (((k.Keyword == "marvel-cinematic-universe") && strings.Contains(n.Name, "Downey")) && strings.Contains(n.Name, "Robert")) && (t.Production_year > 2010) {
								if (((k.Keyword == "marvel-cinematic-universe") && strings.Contains(n.Name, "Downey")) && strings.Contains(n.Name, "Robert")) && (t.Production_year > 2010) {
									results = append(results, Result{
										Movie_keyword: k.Keyword,
										Actor_name:    n.Name,
										Marvel_movie:  t.Title,
									})
								}
							}
						}
					}
				}
			}
		}
		return results
	}()
	func() { b, _ := json.Marshal(result); fmt.Println(string(b)) }()
	test_Q6_finds_marvel_movie_with_Robert_Downey()
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
