package ocaml

import (
	"strings"

	sitter "github.com/smacker/go-tree-sitter"
	tsocaml "github.com/smacker/go-tree-sitter/ocaml"
)

// Program represents a parsed OCaml source file as produced by the
// helper Node script. It mirrors the structure of ocaml_ast.js.
type Program struct {
	Funcs  []Func  `json:"funcs"`
	Prints []Print `json:"prints"`
	Types  []Type  `json:"types"`
	Vars   []Var   `json:"vars"`
}

type Func struct {
	Name    string   `json:"name"`
	Params  []string `json:"params"`
	Body    string   `json:"body"`
	Line    int      `json:"line"`
	Col     int      `json:"col"`
	EndLine int      `json:"endLine"`
	EndCol  int      `json:"endCol"`
	Snippet string   `json:"snippet"`
}

type Var struct {
	Name    string `json:"name"`
	Expr    string `json:"expr"`
	Mutable bool   `json:"mutable"`
	Line    int    `json:"line"`
	Col     int    `json:"col"`
	EndLine int    `json:"endLine"`
	EndCol  int    `json:"endCol"`
	Snippet string `json:"snippet"`
}

type Print struct {
	Expr    string `json:"expr"`
	Line    int    `json:"line"`
	Col     int    `json:"col"`
	EndLine int    `json:"endLine"`
	EndCol  int    `json:"endCol"`
	Snippet string `json:"snippet"`
}

type Type struct {
	Name    string  `json:"name"`
	Fields  []Field `json:"fields"`
	Line    int     `json:"line"`
	Col     int     `json:"col"`
	EndLine int     `json:"endLine"`
	EndCol  int     `json:"endCol"`
	Snippet string  `json:"snippet"`
}

type Field struct {
	Name    string `json:"name"`
	Type    string `json:"type"`
	Line    int    `json:"line"`
	Col     int    `json:"col"`
	EndLine int    `json:"endLine"`
	EndCol  int    `json:"endCol"`
	Snippet string `json:"snippet"`
}

// Inspect parses the given OCaml source code and returns a Program
// describing its structure using the bundled tree-sitter parser.
func Inspect(src string) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(tsocaml.GetLanguage())
	tree := parser.Parse(nil, []byte(src))

	prog := &Program{
		Funcs:  []Func{},
		Prints: []Print{},
		Types:  []Type{},
		Vars:   []Var{},
	}
	root := tree.RootNode()
	for i := 0; i < int(root.NamedChildCount()); i++ {
		child := root.NamedChild(i)
		if child == nil {
			continue
		}
		switch child.Type() {
		case "value_definition", "expression_item":
			prog.Prints = append(prog.Prints, extractPrint(child, []byte(src)))
		}
	}
	return prog, nil
}

func extractPrint(n *sitter.Node, src []byte) Print {
	start := n.StartPoint()
	end := n.EndPoint()
	snippet := n.Content(src)
	expr := snippet

	if n.Type() == "value_definition" && n.NamedChildCount() > 0 {
		lb := n.NamedChild(0)
		for i := 0; i < int(lb.ChildCount()); i++ {
			ch := lb.Child(i)
			if ch != nil && ch.Type() == "=" && i+1 < int(lb.ChildCount()) {
				exprNode := lb.Child(i + 1)
				expr = string(src[exprNode.StartByte():lb.EndByte()])
				break
			}
		}
	}

	return Print{
		Expr:    strings.TrimSpace(expr),
		Line:    int(start.Row) + 1,
		Col:     int(start.Column) + 1,
		EndLine: int(end.Row) + 1,
		EndCol:  int(end.Column) + 1,
		Snippet: snippet,
	}
}
