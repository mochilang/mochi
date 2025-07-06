package any2mochi

import (
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// EnsureAndParse simply invokes ParseText without any setup.
func EnsureAndParse(cmd string, args []string, langID, src string) ([]protocol.DocumentSymbol, []protocol.Diagnostic, error) {
	return ParseText(cmd, args, langID, src)
}

func EnsureAndParseWithRoot(cmd string, args []string, langID, src, root string) ([]protocol.DocumentSymbol, []protocol.Diagnostic, error) {
	return ParseTextWithRoot(cmd, args, langID, src, root)
}

// HoverAt is not supported.
func EnsureAndHover(cmd string, args []string, langID, src string, pos protocol.Position) (protocol.Hover, error) {
	return protocol.Hover{}, nil
}

func EnsureAndHoverWithRoot(cmd string, args []string, langID, src string, pos protocol.Position, root string) (protocol.Hover, error) {
	return protocol.Hover{}, nil
}

func EnsureAndDefinition(cmd string, args []string, langID, src string, pos protocol.Position) ([]protocol.Location, error) {
	return nil, nil
}

func EnsureAndDefinitionWithRoot(cmd string, args []string, langID, src string, pos protocol.Position, root string) ([]protocol.Location, error) {
	return nil, nil
}
