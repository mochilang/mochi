//go:build ignore

// Generated by Mochi compiler v0.10.25 on 2025-07-15T03:06:46Z

package main

import (
	"encoding/json"
	"fmt"
	"reflect"
	"strings"
	"time"

	"golang.org/x/exp/constraints"
)

type Comp_cast_type struct {
	ID   int    `json:"id"`
	Kind string `json:"kind"`
}

type Company_name struct {
	ID           int    `json:"id"`
	Country_code string `json:"country_code"`
}

type Company_type struct {
	ID int `json:"id"`
}

type Complete_cast struct {
	Movie_id  int `json:"movie_id"`
	Status_id int `json:"status_id"`
}

type Info_type struct {
	ID   int    `json:"id"`
	Info string `json:"info"`
}

type Keyword struct {
	ID      int    `json:"id"`
	Keyword string `json:"keyword"`
}

type Kind_type struct {
	ID   int    `json:"id"`
	Kind string `json:"kind"`
}

type Matche struct {
	Movie_kind                 string `json:"movie_kind"`
	Complete_us_internet_movie string `json:"complete_us_internet_movie"`
}

type Movie_companie struct {
	Movie_id        int `json:"movie_id"`
	Company_id      int `json:"company_id"`
	Company_type_id int `json:"company_type_id"`
}

type Movie_info struct {
	Movie_id     int    `json:"movie_id"`
	Info_type_id int    `json:"info_type_id"`
	Note         string `json:"note"`
	Info         string `json:"info"`
}

type Movie_keyword struct {
	Movie_id   int `json:"movie_id"`
	Keyword_id int `json:"keyword_id"`
}

type Result struct {
	Movie_kind                 string `json:"movie_kind"`
	Complete_us_internet_movie string `json:"complete_us_internet_movie"`
}

type Title struct {
	ID              int    `json:"id"`
	Kind_id         int    `json:"kind_id"`
	Production_year int    `json:"production_year"`
	Title           string `json:"title"`
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

func test_Q23_finds_US_internet_movie_with_verified_cast() {
	expect(_equal(result, []v{v{
		Movie_kind:                 "movie",
		Complete_us_internet_movie: "Web Movie",
	}}))
}

var complete_cast []Complete_cast
var comp_cast_type []Comp_cast_type
var company_name []Company_name
var company_type []Company_type
var info_type []Info_type
var keyword []Keyword
var kind_type []Kind_type
var movie_companies []Movie_companie
var movie_info []Movie_info
var movie_keyword []Movie_keyword
var title []Title
var matches []Matche
var result []Result

func main() {
	complete_cast = []Complete_cast{Complete_cast{
		Movie_id:  1,
		Status_id: 1,
	}, Complete_cast{
		Movie_id:  2,
		Status_id: 2,
	}}
	comp_cast_type = []Comp_cast_type{Comp_cast_type{
		ID:   1,
		Kind: "complete+verified",
	}, Comp_cast_type{
		ID:   2,
		Kind: "partial",
	}}
	company_name = []Company_name{Company_name{
		ID:           1,
		Country_code: "[us]",
	}, Company_name{
		ID:           2,
		Country_code: "[gb]",
	}}
	company_type = []Company_type{Company_type{ID: 1}, Company_type{ID: 2}}
	info_type = []Info_type{Info_type{
		ID:   1,
		Info: "release dates",
	}, Info_type{
		ID:   2,
		Info: "other",
	}}
	keyword = []Keyword{Keyword{
		ID:      1,
		Keyword: "internet",
	}, Keyword{
		ID:      2,
		Keyword: "other",
	}}
	kind_type = []Kind_type{Kind_type{
		ID:   1,
		Kind: "movie",
	}, Kind_type{
		ID:   2,
		Kind: "series",
	}}
	movie_companies = []Movie_companie{Movie_companie{
		Movie_id:        1,
		Company_id:      1,
		Company_type_id: 1,
	}, Movie_companie{
		Movie_id:        2,
		Company_id:      2,
		Company_type_id: 2,
	}}
	movie_info = []Movie_info{Movie_info{
		Movie_id:     1,
		Info_type_id: 1,
		Note:         "internet release",
		Info:         "USA: May 2005",
	}, Movie_info{
		Movie_id:     2,
		Info_type_id: 1,
		Note:         "theater",
		Info:         "USA: April 1998",
	}}
	movie_keyword = []Movie_keyword{Movie_keyword{
		Movie_id:   1,
		Keyword_id: 1,
	}, Movie_keyword{
		Movie_id:   2,
		Keyword_id: 2,
	}}
	title = []Title{Title{
		ID:              1,
		Kind_id:         1,
		Production_year: 2005,
		Title:           "Web Movie",
	}, Title{
		ID:              2,
		Kind_id:         1,
		Production_year: 1998,
		Title:           "Old Movie",
	}}
	matches = func() []Matche {
		results := []Matche{}
		for _, ccRaw := range complete_cast {
			cc := ccRaw.(Complete_cast)
			for _, cct1 := range comp_cast_type {
				if !(cct1.ID == cc.Status_id) {
					continue
				}
				for _, t := range title {
					if !(t.ID == cc.Movie_id) {
						continue
					}
					for _, kt := range kind_type {
						if !(kt.ID == t.Kind_id) {
							continue
						}
						for _, mi := range movie_info {
							if !(mi.Movie_id == t.ID) {
								continue
							}
							for _, it1 := range info_type {
								if !(it1.ID == mi.Info_type_id) {
									continue
								}
								for _, mk := range movie_keyword {
									if !(mk.Movie_id == t.ID) {
										continue
									}
									for _, k := range keyword {
										if !(k.ID == mk.Keyword_id) {
											continue
										}
										for _, mc := range movie_companies {
											if !(mc.Movie_id == t.ID) {
												continue
											}
											for _, cn := range company_name {
												if !(cn.ID == mc.Company_id) {
													continue
												}
												if ((((((cct1.Kind == "complete+verified") && (cn.Country_code == "[us]")) && (it1.Info == "release dates")) && (kt.Kind == "movie")) && strings.Contains(mi.Note, "internet")) && (strings.Contains(mi.Info, "USA:") && (strings.Contains(mi.Info, "199") || strings.Contains(mi.Info, "200")))) && (t.Production_year > 2000) {
													for _, ct := range company_type {
														if !(ct.ID == mc.Company_type_id) {
															continue
														}
														results = append(results, Matche{
															Movie_kind:                 kt.Kind,
															Complete_us_internet_movie: t.Title,
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
			}
		}
		return results
	}()
	result = []Result{Result{
		Movie_kind: _minOrdered[string](func() []string {
			results := []string{}
			for _, rRaw := range matches {
				r := rRaw.(Matche)
				results = append(results, r.Movie_kind)
			}
			return results
		}()),
		Complete_us_internet_movie: _minOrdered[string](func() []string {
			results := []string{}
			for _, rRaw := range matches {
				r := rRaw.(Matche)
				results = append(results, r.Complete_us_internet_movie)
			}
			return results
		}()),
	}}
	func() { b, _ := json.Marshal(result); fmt.Println(string(b)) }()
	test_Q23_finds_US_internet_movie_with_verified_cast()
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

func _minOrdered[T constraints.Ordered](s []T) T {
	if len(s) == 0 {
		var zero T
		return zero
	}
	m := s[0]
	for _, v := range s[1:] {
		if v < m {
			m = v
		}
	}
	return m
}
