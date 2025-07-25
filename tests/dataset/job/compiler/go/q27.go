//go:build ignore

// Generated by Mochi compiler v0.10.25 on 2025-07-15T03:06:47Z

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
	Name         string `json:"name"`
	Country_code string `json:"country_code"`
}

type Company_type struct {
	ID   int    `json:"id"`
	Kind string `json:"kind"`
}

type Complete_cast struct {
	Movie_id   int `json:"movie_id"`
	Subject_id int `json:"subject_id"`
	Status_id  int `json:"status_id"`
}

type Keyword struct {
	ID      int    `json:"id"`
	Keyword string `json:"keyword"`
}

type Link_type struct {
	ID   int    `json:"id"`
	Link string `json:"link"`
}

type Matche struct {
	Company string `json:"company"`
	Link    string `json:"link"`
	Title   string `json:"title"`
}

type Movie_companie struct {
	Movie_id        int `json:"movie_id"`
	Company_id      int `json:"company_id"`
	Company_type_id int `json:"company_type_id"`
	Note            any `json:"note"`
}

type Movie_info struct {
	Movie_id int    `json:"movie_id"`
	Info     string `json:"info"`
}

type Movie_keyword struct {
	Movie_id   int `json:"movie_id"`
	Keyword_id int `json:"keyword_id"`
}

type Movie_link struct {
	Movie_id     int `json:"movie_id"`
	Link_type_id int `json:"link_type_id"`
}

type Title struct {
	ID              int    `json:"id"`
	Production_year int    `json:"production_year"`
	Title           string `json:"title"`
}

type v map[string]any

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

func test_Q27_selects_minimal_company__link_and_title() {
	expect(_equal(result, v{
		Producing_company:       "Best Film",
		Link_type:               "follows",
		Complete_western_sequel: "Western Sequel",
	}))
}

var comp_cast_type []Comp_cast_type
var complete_cast []Complete_cast
var company_name []Company_name
var company_type []Company_type
var keyword []Keyword
var link_type []Link_type
var movie_companies []Movie_companie
var movie_info []Movie_info
var movie_keyword []Movie_keyword
var movie_link []Movie_link
var title []Title
var matches []Matche
var result v

func main() {
	comp_cast_type = []Comp_cast_type{Comp_cast_type{
		ID:   1,
		Kind: "cast",
	}, Comp_cast_type{
		ID:   2,
		Kind: "crew",
	}, Comp_cast_type{
		ID:   3,
		Kind: "complete",
	}}
	complete_cast = []Complete_cast{Complete_cast{
		Movie_id:   1,
		Subject_id: 1,
		Status_id:  3,
	}, Complete_cast{
		Movie_id:   2,
		Subject_id: 2,
		Status_id:  3,
	}}
	company_name = []Company_name{Company_name{
		ID:           1,
		Name:         "Best Film",
		Country_code: "[se]",
	}, Company_name{
		ID:           2,
		Name:         "Polish Film",
		Country_code: "[pl]",
	}}
	company_type = []Company_type{Company_type{
		ID:   1,
		Kind: "production companies",
	}, Company_type{
		ID:   2,
		Kind: "other",
	}}
	keyword = []Keyword{Keyword{
		ID:      1,
		Keyword: "sequel",
	}, Keyword{
		ID:      2,
		Keyword: "remake",
	}}
	link_type = []Link_type{Link_type{
		ID:   1,
		Link: "follows",
	}, Link_type{
		ID:   2,
		Link: "related",
	}}
	movie_companies = []Movie_companie{Movie_companie{
		Movie_id:        1,
		Company_id:      1,
		Company_type_id: 1,
		Note:            nil,
	}, Movie_companie{
		Movie_id:        2,
		Company_id:      2,
		Company_type_id: 1,
		Note:            "extra",
	}}
	movie_info = []Movie_info{Movie_info{
		Movie_id: 1,
		Info:     "Sweden",
	}, Movie_info{
		Movie_id: 2,
		Info:     "USA",
	}}
	movie_keyword = []Movie_keyword{Movie_keyword{
		Movie_id:   1,
		Keyword_id: 1,
	}, Movie_keyword{
		Movie_id:   2,
		Keyword_id: 2,
	}}
	movie_link = []Movie_link{Movie_link{
		Movie_id:     1,
		Link_type_id: 1,
	}, Movie_link{
		Movie_id:     2,
		Link_type_id: 2,
	}}
	title = []Title{Title{
		ID:              1,
		Production_year: 1980,
		Title:           "Western Sequel",
	}, Title{
		ID:              2,
		Production_year: 1999,
		Title:           "Another Movie",
	}}
	matches = func() []Matche {
		results := []Matche{}
		for _, ccRaw := range complete_cast {
			cc := ccRaw.(Complete_cast)
			for _, cct1 := range comp_cast_type {
				if !(cct1.ID == cc.Subject_id) {
					continue
				}
				for _, cct2 := range comp_cast_type {
					if !(cct2.ID == cc.Status_id) {
						continue
					}
					for _, t := range title {
						if !(t.ID == cc.Movie_id) {
							continue
						}
						for _, ml := range movie_link {
							if !(ml.Movie_id == t.ID) {
								continue
							}
							for _, lt := range link_type {
								if !(lt.ID == ml.Link_type_id) {
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
											for _, ct := range company_type {
												if !(ct.ID == mc.Company_type_id) {
													continue
												}
												for _, cn := range company_name {
													if !(cn.ID == mc.Company_id) {
														continue
													}
													for _, mi := range movie_info {
														if !(mi.Movie_id == t.ID) {
															continue
														}
														if (((((((((((((((((((((cct1.Kind == "cast") || (cct1.Kind == "crew")) && (cct2.Kind == "complete")) && (cn.Country_code != "[pl]")) && (strings.Contains(cn.Name, "Film") || strings.Contains(cn.Name, "Warner"))) && (ct.Kind == "production companies")) && (k.Keyword == "sequel")) && strings.Contains(lt.Link, "follow")) && _equal(mc.Note, nil)) && ((((mi.Info == "Sweden") || (mi.Info == "Germany")) || (mi.Info == "Swedish")) || (mi.Info == "German"))) && (t.Production_year >= 1950)) && (t.Production_year <= 2000)) && (ml.Movie_id == mk.Movie_id)) && (ml.Movie_id == mc.Movie_id)) && (mk.Movie_id == mc.Movie_id)) && (ml.Movie_id == mi.Movie_id)) && (mk.Movie_id == mi.Movie_id)) && (mc.Movie_id == mi.Movie_id)) && (ml.Movie_id == cc.Movie_id)) && (mk.Movie_id == cc.Movie_id)) && (mc.Movie_id == cc.Movie_id)) && (mi.Movie_id == cc.Movie_id) {
															if (((((((((((((((((((((cct1.Kind == "cast") || (cct1.Kind == "crew")) && (cct2.Kind == "complete")) && (cn.Country_code != "[pl]")) && (strings.Contains(cn.Name, "Film") || strings.Contains(cn.Name, "Warner"))) && (ct.Kind == "production companies")) && (k.Keyword == "sequel")) && strings.Contains(lt.Link, "follow")) && _equal(mc.Note, nil)) && ((((mi.Info == "Sweden") || (mi.Info == "Germany")) || (mi.Info == "Swedish")) || (mi.Info == "German"))) && (t.Production_year >= 1950)) && (t.Production_year <= 2000)) && (ml.Movie_id == mk.Movie_id)) && (ml.Movie_id == mc.Movie_id)) && (mk.Movie_id == mc.Movie_id)) && (ml.Movie_id == mi.Movie_id)) && (mk.Movie_id == mi.Movie_id)) && (mc.Movie_id == mi.Movie_id)) && (ml.Movie_id == cc.Movie_id)) && (mk.Movie_id == cc.Movie_id)) && (mc.Movie_id == cc.Movie_id)) && (mi.Movie_id == cc.Movie_id) {
																results = append(results, Matche{
																	Company: cn.Name,
																	Link:    lt.Link,
																	Title:   t.Title,
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
			}
		}
		return results
	}()
	result = v{
		Producing_company: _minOrdered[string](func() []string {
			results := []string{}
			for _, xRaw := range matches {
				x := xRaw.(Matche)
				results = append(results, x.Company)
			}
			return results
		}()),
		Link_type: _minOrdered[string](func() []string {
			results := []string{}
			for _, xRaw := range matches {
				x := xRaw.(Matche)
				results = append(results, x.Link)
			}
			return results
		}()),
		Complete_western_sequel: _minOrdered[string](func() []string {
			results := []string{}
			for _, xRaw := range matches {
				x := xRaw.(Matche)
				results = append(results, x.Title)
			}
			return results
		}()),
	}
	func() { b, _ := json.Marshal(result); fmt.Println(string(b)) }()
	test_Q27_selects_minimal_company__link_and_title()
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
