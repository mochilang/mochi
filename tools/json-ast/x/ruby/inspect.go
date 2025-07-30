package ruby

// Program represents a parsed Ruby source file.
// Program is the JSON representation returned by Inspect.
type Program struct {
	Root Node `json:"root"`
}

// Inspect parses the given Ruby source code and returns a Program describing
// its AST using the bundled tree-sitter parser.
func Inspect(src string) (*Program, error) {
	node, err := Parse(src)
	if err != nil {
		return nil, err
	}
	return &Program{Root: *node}, nil
}
