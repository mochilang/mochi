package data

// Group represents a group of items with a common key.
type Group struct {
	Key    any
	Items  []any
	Fields map[string]any
}
