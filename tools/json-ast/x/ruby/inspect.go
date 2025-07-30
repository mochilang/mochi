package ruby

// Program represents a parsed Ruby source file.
type Program struct {
	AST *Node `json:"ast"`
}

// Inspect parses the given Ruby source code and returns a Program describing
// its AST using the bundled tree-sitter parser.
func Inspect(src string) (*Program, error) {
	node, err := Parse(src)
	if err != nil {
		return nil, err
	}
	node.Source = ""
	return &Program{AST: node}, nil
}
