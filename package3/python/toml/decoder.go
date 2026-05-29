package toml

import (
	"fmt"
)

// Decoder is a typed accessor over a parsed TOML map tree. It returns explicit
// errors instead of panicking on type mismatches, so the bridge sub-packages
// can decode upstream files (uv.lock, pylock.toml) defensively.
type Decoder struct {
	tree map[string]any
}

// NewDecoder wraps a parsed tree.
func NewDecoder(tree map[string]any) *Decoder {
	return &Decoder{tree: tree}
}

// String returns the string at key, or an error if the key is missing or has
// the wrong type. Returns ("", nil) when ok=false and the key is absent.
func (d *Decoder) String(key string) (string, bool, error) {
	v, ok := d.tree[key]
	if !ok {
		return "", false, nil
	}
	s, ok := v.(string)
	if !ok {
		return "", true, fmt.Errorf("toml: key %q is %T, want string", key, v)
	}
	return s, true, nil
}

// StringRequired is String but returns an error when the key is absent.
func (d *Decoder) StringRequired(key string) (string, error) {
	s, present, err := d.String(key)
	if err != nil {
		return "", err
	}
	if !present {
		return "", fmt.Errorf("toml: missing required key %q", key)
	}
	return s, nil
}

// Int returns the int64 at key. Returns (0, false, nil) when absent.
func (d *Decoder) Int(key string) (int64, bool, error) {
	v, ok := d.tree[key]
	if !ok {
		return 0, false, nil
	}
	n, ok := v.(int64)
	if !ok {
		return 0, true, fmt.Errorf("toml: key %q is %T, want int64", key, v)
	}
	return n, true, nil
}

// Bool returns the bool at key. Returns (false, false, nil) when absent.
func (d *Decoder) Bool(key string) (bool, bool, error) {
	v, ok := d.tree[key]
	if !ok {
		return false, false, nil
	}
	b, ok := v.(bool)
	if !ok {
		return false, true, fmt.Errorf("toml: key %q is %T, want bool", key, v)
	}
	return b, true, nil
}

// Table returns the sub-table at key. Returns (nil, false, nil) when absent.
func (d *Decoder) Table(key string) (*Decoder, bool, error) {
	v, ok := d.tree[key]
	if !ok {
		return nil, false, nil
	}
	t, ok := v.(map[string]any)
	if !ok {
		return nil, true, fmt.Errorf("toml: key %q is %T, want table", key, v)
	}
	return &Decoder{tree: t}, true, nil
}

// TableArray returns an array-of-tables at key. Returns (nil, false, nil) when
// absent.
func (d *Decoder) TableArray(key string) ([]*Decoder, bool, error) {
	v, ok := d.tree[key]
	if !ok {
		return nil, false, nil
	}
	arr, ok := v.([]map[string]any)
	if !ok {
		return nil, true, fmt.Errorf("toml: key %q is %T, want array-of-tables", key, v)
	}
	out := make([]*Decoder, len(arr))
	for i, t := range arr {
		out[i] = &Decoder{tree: t}
	}
	return out, true, nil
}

// StringArray returns a []string at key. Returns (nil, false, nil) when absent.
// Returns an error when the array has a non-string element.
func (d *Decoder) StringArray(key string) ([]string, bool, error) {
	v, ok := d.tree[key]
	if !ok {
		return nil, false, nil
	}
	switch arr := v.(type) {
	case []any:
		out := make([]string, len(arr))
		for i, it := range arr {
			s, ok := it.(string)
			if !ok {
				return nil, true, fmt.Errorf("toml: key %q index %d is %T, want string", key, i, it)
			}
			out[i] = s
		}
		return out, true, nil
	case []map[string]any:
		return nil, true, fmt.Errorf("toml: key %q is an array-of-tables, want string array", key)
	default:
		return nil, true, fmt.Errorf("toml: key %q is %T, want string array", key, v)
	}
}

// Raw returns the underlying tree for callers that need to inspect arbitrary
// values.
func (d *Decoder) Raw() map[string]any {
	return d.tree
}

// Keys returns the set of keys in the wrapped table in unspecified order.
func (d *Decoder) Keys() []string {
	out := make([]string, 0, len(d.tree))
	for k := range d.tree {
		out = append(out, k)
	}
	return out
}
