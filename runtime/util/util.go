package util

import (
	"encoding/json"
	"fmt"
	"reflect"
	"strconv"
)

// ToAnyMap converts various map representations to map[string]any.
func ToAnyMap(m any) map[string]any {
	switch v := m.(type) {
	case map[string]any:
		return v
	case map[string]string:
		out := make(map[string]any, len(v))
		for k, vv := range v {
			out[k] = vv
		}
		return out
	case map[int]any:
		out := make(map[string]any, len(v))
		for k, vv := range v {
			out[strconv.Itoa(k)] = vv
		}
		return out
	case map[any]any:
		out := make(map[string]any, len(v))
		for kk, vv := range v {
			out[fmt.Sprint(kk)] = vv
		}
		return out
	default:
		return nil
	}
}

// ToMapSlice attempts to coerce v into a slice of map[string]any.
func ToMapSlice(v any) ([]map[string]any, bool) {
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
