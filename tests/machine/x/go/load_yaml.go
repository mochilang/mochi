//go:build ignore

package main

import (
	"fmt"
	"mochi/runtime/data"
	"os"
	"reflect"
	"strings"
)

type Person struct {
	Name  string `json:"name"`
	Age   int    `json:"age"`
	Email string `json:"email"`
}

func main() {
	type v struct {
		Format string `json:"format"`
	}

	var people []Person = func() []Person {
		rows := _load("../../../tests/interpreter/valid/people.yaml", _toAnyMap(v{"yaml"}))
		out := make([]Person, len(rows))
		for i, r := range rows {
			out[i] = r.(Person)
		}
		return out
	}()
	type Adults struct {
		Name  any `json:"name"`
		Email any `json:"email"`
	}

	var adults []Adults = func() []Adults {
		results := []Adults{}
		for _, p := range people {
			if p.Age >= 18 {
				if p.Age >= 18 {
					results = append(results, Adults{
						p.Name,
						p.Email,
					})
				}
			}
		}
		return results
	}()
	for _, a := range adults {
		fmt.Println(a.Name, a.Email)
	}
}

func _load(path string, opts map[string]any) []map[string]any {
	format := "csv"
	header := false
	delim := ','
	if opts != nil {
		if f, ok := opts["format"].(string); ok {
			format = f
		}
		if h, ok := opts["header"].(bool); ok {
			header = h
		}
		if d, ok := opts["delimiter"].(string); ok && len(d) > 0 {
			delim = rune(d[0])
		}
	}
	var rows []map[string]any
	var err error
	switch format {
	case "jsonl":
		if path == "" || path == "-" {
			rows, err = data.LoadJSONLReader(os.Stdin)
		} else {
			rows, err = data.LoadJSONL(path)
		}
	case "json":
		if path == "" || path == "-" {
			rows, err = data.LoadJSONReader(os.Stdin)
		} else {
			rows, err = data.LoadJSON(path)
		}
	case "yaml":
		if path == "" || path == "-" {
			rows, err = data.LoadYAMLReader(os.Stdin)
		} else {
			rows, err = data.LoadYAML(path)
		}
	case "tsv":
		delim = '	'
		fallthrough
	default:
		if path == "" || path == "-" {
			rows, err = data.LoadCSVReader(os.Stdin, header, delim)
		} else {
			rows, err = data.LoadCSV(path, header, delim)
		}
	}
	if err != nil {
		panic(err)
	}
	return rows
}

func _toAnyMap(m any) map[string]any {
	switch v := m.(type) {
	case map[string]any:
		return v
	case map[string]string:
		out := make(map[string]any, len(v))
		for k, vv := range v {
			out[k] = vv
		}
		return out
	default:
		rv := reflect.ValueOf(v)
		if rv.Kind() == reflect.Struct {
			out := make(map[string]any, rv.NumField())
			rt := rv.Type()
			for i := 0; i < rv.NumField(); i++ {
				name := rt.Field(i).Name
				if tag := rt.Field(i).Tag.Get("json"); tag != "" {
					comma := strings.Index(tag, ",")
					if comma >= 0 {
						tag = tag[:comma]
					}
					if tag != "-" {
						name = tag
					}
				}
				out[name] = rv.Field(i).Interface()
			}
			return out
		}
		return nil
	}
}
