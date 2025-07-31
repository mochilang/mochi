package ruby

// Program represents a parsed Ruby source file.
// Program is the JSON representation returned by Inspect.
type Program struct {
	Root *ProgramNode `json:"root"`
}

// Inspect parses the given Ruby source code and returns a Program describing
// its AST using the bundled tree-sitter parser.
// Inspect parses the given Ruby source code and returns a Program describing
// its AST using the bundled tree-sitter parser. The AST retains operator tokens
// and control flow keywords so that Print can faithfully reconstruct the
// original program. Positional information is omitted unless opts specifies
// IncludePositions.
func Inspect(src string, opts ...Options) (*Program, error) {
	node, err := Parse(src, opts...)
	if err != nil {
		return nil, err
	}
	if node == nil {
		return &Program{Root: nil}, nil
	}
	root := &ProgramNode{Node: *node}
	return &Program{Root: root}, nil
}
