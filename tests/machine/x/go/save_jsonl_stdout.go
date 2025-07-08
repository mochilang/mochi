//go:build ignore

package main

import (
	"encoding/json"
	"mochi/runtime/data"
	"os"
	"reflect"
)

func main() {
	type PeopleItem struct {
		Name string `json:"name"`
		Age  int    `json:"age"`
	}

	var people []PeopleItem = []PeopleItem{PeopleItem{
		Name: "Alice",
		Age:  30,
	}, PeopleItem{
		Name: "Bob",
		Age:  25,
	}}
	_save(people, "-", _toAnyMap(map[string]string{"format": "jsonl"}))
}

func _save(src any, path string, opts map[string]any) {
	rows, ok := _toMapSlice(src)
	if !ok {
		panic("save source must be list of maps")
	}
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
	var err error
	switch format {
	case "jsonl":
		if path == "" || path == "-" {
			err = data.SaveJSONLWriter(rows, os.Stdout)
		} else {
			err = data.SaveJSONL(rows, path)
		}
	case "json":
		if path == "" || path == "-" {
			err = data.SaveJSONWriter(rows, os.Stdout)
		} else {
			err = data.SaveJSON(rows, path)
		}
	case "yaml":
		if path == "" || path == "-" {
			err = data.SaveYAMLWriter(rows, os.Stdout)
		} else {
			err = data.SaveYAML(rows, path)
		}
	case "tsv":
		delim = '	'
		fallthrough
	default:
		if path == "" || path == "-" {
			err = data.SaveCSVWriter(rows, os.Stdout, header, delim)
		} else {
			err = data.SaveCSV(rows, path, header, delim)
		}
	}
	if err != nil {
		panic(err)
	}
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

func _toMapSlice(v any) ([]map[string]any, bool) {
	switch rows := v.(type) {
	case []map[string]any:
		return rows, true
	case []any:
		out := make([]map[string]any, len(rows))
		for i, item := range rows {
			m, ok := item.(map[string]any)
			if !ok {
				return nil, false
			}
			out[i] = m
		}
		return out, true
	}
	rv := reflect.ValueOf(v)
	if rv.Kind() == reflect.Slice {
		out := make([]map[string]any, rv.Len())
		for i := 0; i < rv.Len(); i++ {
			b, err := json.Marshal(rv.Index(i).Interface())
			if err != nil {
				return nil, false
			}
			var m map[string]any
			if err := json.Unmarshal(b, &m); err != nil {
				return nil, false
			}
			out[i] = m
		}
		return out, true
	}
	return nil, false
}
