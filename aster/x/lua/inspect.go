package lua

import (
	"context"

	tslua "github.com/tree-sitter-grammars/tree-sitter-lua/bindings/go"
	sitter "github.com/tree-sitter/go-tree-sitter"
)

// Program describes a parsed Lua source file.
type Program struct {
	Root *ProgramNode `json:"root"`
}

// Inspect parses Lua source code using tree-sitter and returns its Program.
func Inspect(src string) (*Program, error) {
	return InspectWithPositions(src, false)
}

// InspectWithPositions parses Lua source and optionally includes position information.
func InspectWithPositions(src string, withPos bool) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(sitter.NewLanguage(tslua.Language()))
	tree := parser.ParseCtx(context.Background(), []byte(src), nil)
	return convertProgram(tree.RootNode(), []byte(src), withPos), nil
}
