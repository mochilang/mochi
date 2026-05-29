// Package rbs - YARD fallback ingest (phase 4 of MEP-76).
//
// When a gem ships no bundled RBS files and has no gem_rbs_collection entry,
// the bridge invokes `yard doc --format=json` and parses the resulting JSON
// document. YARD type strings are freeform and not grammar-validated, so the
// parser here uses a best-effort approach for the common subset.
package rbs

import (
	"encoding/json"
	"strings"
)

// yardDoc is the top-level shape of the JSON produced by
// `yard doc --format=json`.
type yardDoc struct {
	DocObjects []yardObject `json:"docobjects"`
}

// yardObject is one entry in the docobjects array.
type yardObject struct {
	Path       string        `json:"path"`
	Type       string        `json:"type"`
	Name       string        `json:"name"`
	Parameters [][]string    `json:"parameters"` // each element: [name, type_string]
	ReturnType string        `json:"return_type"`
	Namespace  *yardNS       `json:"namespace"`
}

// yardNS is the namespace descriptor embedded in each docobject.
type yardNS struct {
	Path string `json:"path"`
	Type string `json:"type"` // "class" or "module"
}

// ParseYARDType converts a YARD type annotation string to a Type node.
// Returns a node with TypeUnknown kind if the string is not recognisable.
// YARD type strings are freeform; this parser handles the common subset.
func ParseYARDType(s string) *Type {
	s = strings.TrimSpace(s)
	if s == "" {
		return &Type{Kind: TypeUnknown}
	}

	// Try a single-token parse first. If the string contains a comma but is
	// a recognised container type (e.g. "Hash<String, Integer>"), the single-
	// token parser handles the comma internally.
	if t := parseSingleYARDType(s); t != nil && t.Kind != TypeUnknown {
		return t
	}

	// Not a single recognised type. If there is a top-level comma (outside
	// any angle or brace brackets) treat it as a union.
	if !hasTopLevelComma(s) {
		return &Type{Kind: TypeUnknown}
	}

	// Check for union with nil: "Foo, nil" or "nil, Foo" or "true, false".
	if t := parseOptionalUnion(s); t != nil {
		return t
	}

	// Two-token comma-separated non-nil union (e.g. "Integer, Float") is complex.
	return &Type{Kind: TypeUnknown}
}

// hasTopLevelComma reports whether s contains a comma that is not enclosed
// inside angle brackets (<>) or braces ({}).
func hasTopLevelComma(s string) bool {
	depth := 0
	for _, ch := range s {
		switch ch {
		case '<', '{':
			depth++
		case '>', '}':
			depth--
		case ',':
			if depth == 0 {
				return true
			}
		}
	}
	return false
}

// parseOptionalUnion handles "T, nil" and "nil, T" patterns.
func parseOptionalUnion(s string) *Type {
	parts := strings.SplitN(s, ",", 2)
	if len(parts) != 2 {
		return nil
	}
	a := strings.TrimSpace(parts[0])
	b := strings.TrimSpace(parts[1])

	// "Boolean" is a special YARD pseudo-type; "true, false" must not be
	// treated as an optional because neither side is nil.
	if (a == "true" && b == "false") || (a == "false" && b == "true") {
		return &Type{Kind: TypeBool}
	}

	// T, nil  or  nil, T
	var inner string
	switch {
	case b == "nil":
		inner = a
	case a == "nil":
		inner = b
	default:
		return nil
	}

	elem := parseSingleYARDType(inner)
	if elem == nil || elem.Kind == TypeUnknown {
		return &Type{Kind: TypeUnknown}
	}
	return &Type{Kind: TypeOptional, Elem: elem}
}

// parseSingleYARDType handles a single (non-union) YARD type token.
func parseSingleYARDType(s string) *Type {
	s = strings.TrimSpace(s)
	switch s {
	case "String":
		return &Type{Kind: TypeString}
	case "Integer":
		return &Type{Kind: TypeInteger}
	case "Float":
		return &Type{Kind: TypeFloat}
	case "Symbol":
		return &Type{Kind: TypeSymbol}
	case "Boolean":
		return &Type{Kind: TypeBool}
	case "nil":
		return &Type{Kind: TypeNil}
	case "void":
		return &Type{Kind: TypeVoid}
	case "Numeric":
		// YARD sometimes uses Numeric as a supertype; we map to float as a
		// conservative approximation.
		return &Type{Kind: TypeFloat}
	}

	// Array<Elem>
	if strings.HasPrefix(s, "Array<") && strings.HasSuffix(s, ">") {
		inner := strings.TrimSpace(s[len("Array<") : len(s)-1])
		elem := parseSingleYARDType(inner)
		if elem == nil {
			return &Type{Kind: TypeUnknown}
		}
		return &Type{Kind: TypeArray, Elem: elem}
	}

	// Hash{Key => Value}  (YARD brace notation)
	if strings.HasPrefix(s, "Hash{") && strings.HasSuffix(s, "}") {
		inner := s[len("Hash{") : len(s)-1]
		return parseHashInner(inner, "=>")
	}

	// Hash<Key, Value>  (alternative angle-bracket notation)
	if strings.HasPrefix(s, "Hash<") && strings.HasSuffix(s, ">") {
		inner := s[len("Hash<") : len(s)-1]
		return parseHashInner(inner, ",")
	}

	// Anything else is unrecognised.
	return &Type{Kind: TypeUnknown}
}

// parseHashInner splits hash key/value on the given separator and builds a
// TypeHash node.
func parseHashInner(inner, sep string) *Type {
	idx := strings.Index(inner, sep)
	if idx < 0 {
		return &Type{Kind: TypeUnknown}
	}
	rawKey := strings.TrimSpace(inner[:idx])
	rawVal := strings.TrimSpace(inner[idx+len(sep):])
	key := parseSingleYARDType(rawKey)
	val := parseSingleYARDType(rawVal)
	if key == nil || val == nil || key.Kind == TypeUnknown || val.Kind == TypeUnknown {
		return &Type{Kind: TypeUnknown}
	}
	return &Type{Kind: TypeHash, Key: key, Value: val}
}

// ParseYARDDoc parses a minimal YARD JSON document (from `yard doc --format=json`)
// and returns a GemSurface with Source=SourceYARD.
//
// The JSON shape has a `docobjects` array where each object may have type,
// name, path, parameters, return_type, and namespace fields.
func ParseYARDDoc(gem, version string, jsonData []byte) (*GemSurface, error) {
	var doc yardDoc
	if err := json.Unmarshal(jsonData, &doc); err != nil {
		return nil, err
	}

	// Collect methods grouped by class name.
	classMethods := make(map[string][]MethodDecl)
	classOrder := []string{}
	seen := make(map[string]bool)

	for _, obj := range doc.DocObjects {
		if obj.Type != "method" {
			continue
		}

		className := ""
		if obj.Namespace != nil {
			className = obj.Namespace.Path
		}
		if className == "" {
			// Derive from path "ClassName#method" or "ClassName.method".
			if i := strings.IndexAny(obj.Path, "#."); i >= 0 {
				className = obj.Path[:i]
			} else {
				className = obj.Path
			}
		}

		if !seen[className] {
			seen[className] = true
			classOrder = append(classOrder, className)
		}

		params := make([]Param, 0, len(obj.Parameters))
		for _, p := range obj.Parameters {
			name := ""
			typeStr := ""
			if len(p) >= 1 {
				name = p[0]
			}
			if len(p) >= 2 {
				typeStr = p[1]
			}
			t := ParseYARDType(typeStr)
			params = append(params, Param{Name: name, Type: t})
		}

		retType := ParseYARDType(obj.ReturnType)

		mt := MethodType{
			Params: params,
			Return: retType,
		}
		decl := MethodDecl{
			Name:  obj.Name,
			Kind:  MethodInstance,
			Types: []MethodType{mt},
		}

		// Detect singleton methods from the path separator.
		if strings.Contains(obj.Path, ".") && !strings.Contains(obj.Path, "#") {
			decl.Kind = MethodSingleton
		}

		classMethods[className] = append(classMethods[className], decl)
	}

	classes := make([]ClassDecl, 0, len(classOrder))
	for _, name := range classOrder {
		classes = append(classes, ClassDecl{
			Name:    name,
			Methods: classMethods[name],
		})
	}

	return &GemSurface{
		Gem:     gem,
		Version: version,
		Source:  SourceYARD,
		Classes: classes,
	}, nil
}
