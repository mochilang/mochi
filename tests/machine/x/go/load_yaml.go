//go:build ignore

package main

import (
	"fmt"
	"mochi/runtime/data"
	"os"
	"strings"
)

type Person struct {
	Name  string `json:"name"`
	Age   int    `json:"age"`
	Email string `json:"email"`
}

func main() {
	var people []Person = func() []Person {
		rows := _load("../../../tests/interpreter/valid/people.yaml", _toAnyMap(map[string]string{"format": "yaml"}))
		out := make([]Person, len(rows))
		for i, r := range rows {
			out[i] = _cast[Person](r)
		}
		return out
	}()
	var adults []map[string]string = func() []map[string]string {
		_res := []map[string]string{}
		for _, p := range people {
			if p.Age >= 18 {
				if p.Age >= 18 {
					_res = append(_res, map[string]string{"name": p.Name, "email": p.Email})
				}
			}
		}
		return _res
	}()
	for _, a := range adults {
		fmt.Println(strings.TrimRight(strings.Join([]string{fmt.Sprint(a["name"]), fmt.Sprint(a["email"])}, " "), " "))
	}
}

func _cast[T any](v any) T {
	return v.(T)
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
		return nil
	}
}
