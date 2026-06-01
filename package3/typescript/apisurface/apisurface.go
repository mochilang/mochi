// Package apisurface is the bridge-side parser for the ApiSurface JSON document
// that the ts-ingest Node.js helper emits. Lands in MEP-72 Phase 4.
package apisurface

import (
	"encoding/json"
	"fmt"
	"os"
)

// ExportKind classifies a TypeScript export.
type ExportKind string

const (
	KindFunction  ExportKind = "function"
	KindClass     ExportKind = "class"
	KindInterface ExportKind = "interface"
	KindType      ExportKind = "type"
	KindConst     ExportKind = "const"
	KindEnum      ExportKind = "enum"
)

// Param is one function parameter.
type Param struct {
	Name     string `json:"name"`
	TypeStr  string `json:"type"`
	Optional bool   `json:"optional,omitempty"`
}

// Export is one declaration from a TypeScript module.
type Export struct {
	Kind        ExportKind `json:"kind"`
	Name        string     `json:"name"`
	Params      []Param    `json:"params,omitempty"`
	ReturnType  string     `json:"returnType,omitempty"`
	TypeParams  []string   `json:"typeParams,omitempty"`
	IsAsync     bool       `json:"isAsync,omitempty"`
	Deprecated  bool       `json:"deprecated,omitempty"`
	Internal    bool       `json:"internal,omitempty"`
	BrowserOnly bool       `json:"browserOnly,omitempty"`
}

// ApiSurface is the top-level JSON document from ts-ingest.
type ApiSurface struct {
	PackageName string               `json:"packageName"`
	Version     string               `json:"version"`
	Exports     map[string][]Export  `json:"exports"`
}

// Parse parses an ApiSurface JSON document from raw bytes.
func Parse(data []byte) (*ApiSurface, error) {
	var s ApiSurface
	if err := json.Unmarshal(data, &s); err != nil {
		return nil, fmt.Errorf("apisurface: parse: %w", err)
	}
	if s.PackageName == "" {
		return nil, fmt.Errorf("apisurface: missing packageName")
	}
	return &s, nil
}

// ParseFile reads and parses an ApiSurface JSON file.
func ParseFile(path string) (*ApiSurface, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("apisurface: read %s: %w", path, err)
	}
	return Parse(data)
}

// AllExports returns a flat slice of all exports across all module paths.
func (s *ApiSurface) AllExports() []Export {
	var out []Export
	for _, exports := range s.Exports {
		out = append(out, exports...)
	}
	return out
}
