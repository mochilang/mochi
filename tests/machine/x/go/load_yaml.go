//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
	"mochi/runtime/data"
	"os"
	"strconv"
	"strings"
)

type Person struct {
	Name  string `json:"name"`
	Age   int    `json:"age"`
	Email string `json:"email"`
}

func main() {
	var people []Person = func() []Person {
		rows := _load("../interpreter/valid/people.yaml", _toAnyMap(map[string]string{"format": "yaml"}))
		out := make([]Person, len(rows))
		for i, r := range rows {
			out[i] = _cast[Person](r)
		}
		return out
	}()
	type Adults struct {
		Name  any `json:"name"`
		Email any `json:"email"`
	}

	type Result struct {
		Name  string `json:"name"`
		Email string `json:"email"`
	}

	var adults []Adults = _cast[[]Adults](func() []Result {
		_res := []Result{}
		for _, p := range people {
			if p.Age >= 18 {
				if p.Age >= 18 {
					_res = append(_res, Result{
						Name:  p.Name,
						Email: p.Email,
					})
				}
			}
		}
		return _res
	}())
	for _, a := range adults {
		fmt.Println(strings.TrimRight(strings.Join([]string{fmt.Sprint(a.Name), fmt.Sprint(a.Email)}, " "), " "))
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
		case string:
			n, _ := strconv.Atoi(vv)
			return any(n).(T)
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
