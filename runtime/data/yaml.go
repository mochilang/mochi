package data

import (
	"fmt"
	"io"

	"gopkg.in/yaml.v3"
)

// LoadYAML reads a YAML file containing an object or array of objects.
func LoadYAML(path string) ([]map[string]any, error) {
	r, closeFn, err := openReader(path)
	if err != nil {
		return nil, err
	}
	defer closeFn()
	return LoadYAMLReader(r)
}

// LoadYAMLReader reads YAML data from r and returns rows.
func LoadYAMLReader(r io.Reader) ([]map[string]any, error) {
	dec := yaml.NewDecoder(r)
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
				return nil, fmt.Errorf("yaml array element must be object")
			}
			out = append(out, m)
		}
		return out, nil
	case map[string]any:
		return []map[string]any{v}, nil
	default:
		return nil, fmt.Errorf("unsupported yaml value %T", data)
	}
}

// SaveYAML writes rows to a YAML file.
func SaveYAML(rows []map[string]any, path string) error {
	w, closeFn, err := openWriter(path)
	if err != nil {
		return err
	}
	defer closeFn()
	return SaveYAMLWriter(rows, w)
}

// SaveYAMLWriter writes rows in YAML format to w.
func SaveYAMLWriter(rows []map[string]any, w io.Writer) error {
	enc := yaml.NewEncoder(w)
	defer enc.Close()
	if len(rows) == 1 {
		return enc.Encode(rows[0])
	}
	return enc.Encode(rows)
}
