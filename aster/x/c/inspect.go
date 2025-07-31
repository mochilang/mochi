package c

import (
	"context"
	"encoding/json"

	sitter "github.com/tree-sitter/go-tree-sitter"
	tsc "github.com/tree-sitter/tree-sitter-c/bindings/go"
)

// Program is the root of a parsed C translation unit.
// Program represents a parsed C translation unit.
type Program struct {
	Root *TranslationUnit `json:"root"`
}

// Inspect parses the given C source code using tree-sitter and returns
// its Program structure.
// Inspect parses the given C source code using tree-sitter and returns
// its Program structure without position information.
func Inspect(src string) (*Program, error) {
	return InspectWithOption(src, Option{Positions: IncludePos})
}

// InspectWithPositions parses the C source and keeps position fields in the AST.
func InspectWithPositions(src string) (*Program, error) {
	return InspectWithOption(src, Option{Positions: true})
}

// InspectWithOption behaves like Inspect but allows callers to specify whether
// position information should be included in the resulting AST.
func InspectWithOption(src string, opt Option) (*Program, error) {
	return inspect(src, opt.Positions)
}

func inspect(src string, pos bool) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(sitter.NewLanguage(tsc.Language()))
	tree := parser.ParseCtx(context.Background(), []byte(src), nil)
	root := convert(tree.RootNode(), []byte(src), pos)
	fixTrailing(root)
	return &Program{Root: (*TranslationUnit)(root)}, nil
}

func fixTrailing(n *Node) {
	if n == nil {
		return
	}
	var fn *Node
	var idx int
	count := 0
	for i, c := range n.Children {
		if c.Kind == "function_definition" {
			fn = c
			idx = i
			count++
		}
	}
	if count != 1 || fn == nil || len(fn.Children) < 3 || len(n.Children) <= idx+1 {
		return
	}
	body := fn.Children[2]
	if body.Kind != "compound_statement" {
		return
	}
	body.Children = append(body.Children, n.Children[idx+1:]...)
	n.Children = n.Children[:idx+1]
}

// MarshalJSON ensures stable output ordering.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
