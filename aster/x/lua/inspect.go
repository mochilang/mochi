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
// Position information is omitted unless opt.Positions is true.
func Inspect(src string) (*Program, error) {
	return InspectWithOption(src, Option{})
}

// InspectWithOption behaves like Inspect but allows callers to include
// positional information in the resulting AST.
func InspectWithOption(src string, opt Option) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(sitter.NewLanguage(tslua.Language()))
	tree := parser.ParseCtx(context.Background(), []byte(src), nil)
	prog := convertProgram(tree.RootNode(), []byte(src), opt)
	return prog, nil
}

// InspectWithPositions is kept for backward compatibility. When called the
// resulting AST will include positional information.
func InspectWithPositions(src string) (*Program, error) {
	return InspectWithOption(src, Option{Positions: true})
}
