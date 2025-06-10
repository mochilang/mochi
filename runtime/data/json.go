package data

import (
	"encoding/json"
	"fmt"
	"io"
)

// LoadJSON reads a JSON file containing either an object or array of objects.
func LoadJSON(path string) ([]map[string]any, error) {
	r, closeFn, err := openReader(path)
	if err != nil {
		return nil, err
	}
	defer closeFn()
	return LoadJSONReader(r)
}

// LoadJSONReader reads JSON data from r and returns rows.
func LoadJSONReader(r io.Reader) ([]map[string]any, error) {
	dec := json.NewDecoder(r)
	var data any
	if err := dec.Decode(&data); err != nil {
		return nil, err
	}
	switch v := data.(type) {
	case []any:
		out := make([]map[string]any, 0, len(v))
		for _, it := range v {
			m, ok := it.(map[string]any)
			if !ok {
				return nil, fmt.Errorf("json array element must be object")
			}
			out = append(out, m)
		}
		return out, nil
	case map[string]any:
		return []map[string]any{v}, nil
	default:
		return nil, fmt.Errorf("unsupported json value %T", data)
	}
}

// SaveJSON writes rows to a JSON file as an array or single object.
func SaveJSON(rows []map[string]any, path string) error {
	w, closeFn, err := openWriter(path)
	if err != nil {
		return err
	}
	defer closeFn()
	return SaveJSONWriter(rows, w)
}

// SaveJSONWriter writes rows to w in JSON format.
func SaveJSONWriter(rows []map[string]any, w io.Writer) error {
	enc := json.NewEncoder(w)
	if len(rows) == 1 {
		return enc.Encode(rows[0])
	}
	return enc.Encode(rows)
}
