package clj

import (
	sitter "github.com/smacker/go-tree-sitter"
	cljlang "github.com/sogaiu/tree-sitter-clojure/bindings/go"
)

// Inspect parses the given Clojure source code using tree-sitter and returns its Program.
func Inspect(src string) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(cljlang.GetLanguage())
	tree := parser.Parse(nil, []byte(src))
	root := tree.RootNode()
	var forms []*Node
	for i := 0; i < int(root.NamedChildCount()); i++ {
		child := root.NamedChild(i)
		if child == nil {
			continue
		}
		forms = append(forms, convertNode(child, []byte(src)))
	}
	return &Program{Forms: forms}, nil
}
