//go:build slow

package any2mochi

import ()

// EnsureAndParse simply invokes ParseText without any setup.
func EnsureAndParse(cmd string, args []string, langID, src string) ([]DocumentSymbol, []Diagnostic, error) {
	return ParseText(cmd, args, langID, src)
}

func EnsureAndParseWithRoot(cmd string, args []string, langID, src, root string) ([]DocumentSymbol, []Diagnostic, error) {
	return ParseTextWithRoot(cmd, args, langID, src, root)
}

// HoverAt is not supported.
func EnsureAndHover(cmd string, args []string, langID, src string, pos Position) (Hover, error) {
	return Hover{}, nil
}

func EnsureAndHoverWithRoot(cmd string, args []string, langID, src string, pos Position, root string) (Hover, error) {
	return Hover{}, nil
}

func EnsureAndDefinition(cmd string, args []string, langID, src string, pos Position) ([]Location, error) {
	return nil, nil
}

func EnsureAndDefinitionWithRoot(cmd string, args []string, langID, src string, pos Position, root string) ([]Location, error) {
	return nil, nil
}
