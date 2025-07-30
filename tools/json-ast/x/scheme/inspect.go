package scheme

// Inspect parses Scheme source code using tree-sitter-racket and returns
// its Program representation.
func Inspect(src string) (*Program, error) {
	return &Program{Root: parse(src)}, nil
}
