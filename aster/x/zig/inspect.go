//go:build slow

package zig

import (
	"context"
	"encoding/json"
	"os"

	tsz "github.com/tree-sitter-grammars/tree-sitter-zig/bindings/go"
	sitter "github.com/tree-sitter/go-tree-sitter"
)

// Program describes a parsed Zig source file.
type Program struct {
	Path string      `json:"path,omitempty"`
	Root *SourceFile `json:"root"`
}

// MarshalJSON implements json.Marshaler for Program to provide stable output.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}

// Inspect parses Zig source code using tree-sitter.
// By default the resulting AST omits position information. Set opts.Positions
// to true to include it.
func Inspect(src string, opts ...Options) (*Program, error) {
	var opt Options
	if len(opts) > 0 {
		opt = opts[0]
	}
	parser := sitter.NewParser()
	parser.SetLanguage(sitter.NewLanguage(tsz.Language()))
	tree := parser.ParseCtx(context.Background(), []byte(src), nil)
	node, ok := convertNode(tree.RootNode(), []byte(src), opt.Positions)
	if !ok {
		return &Program{Root: nil}, nil
	}
	root := SourceFile(node)
	return &Program{Root: &root}, nil
}

// InspectFile reads the Zig source from path and parses it using Inspect.
func InspectFile(path string, opts ...Options) (*Program, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	prog, err := Inspect(string(data), opts...)
	if err != nil {
		return nil, err
	}
	prog.Path = path
	return prog, nil
}
