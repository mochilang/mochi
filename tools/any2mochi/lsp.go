package any2mochi

import (
	"fmt"
	protocol "github.com/tliron/glsp/protocol_3_16"
	"strings"
)

// EnsureAndParse runs ParseText after ensuring the language server is installed.
// Any parsing errors are annotated with a short snippet of the source for easier debugging.
func EnsureAndParse(cmd string, args []string, langID, src string) ([]protocol.DocumentSymbol, []protocol.Diagnostic, error) {
	return EnsureAndParseWithRoot(cmd, args, langID, src, "")
}

// EnsureAndParseWithRoot runs ParseTextWithRoot after ensuring the language server is installed.
func EnsureAndParseWithRoot(cmd string, args []string, langID, src, root string) ([]protocol.DocumentSymbol, []protocol.Diagnostic, error) {
	if err := EnsureServer(cmd); err != nil {
		return nil, nil, err
	}
	syms, diags, err := ParseTextWithRoot(cmd, args, langID, src, root)
	if err != nil {
		err = fmt.Errorf("parse failure: %w\n\nsource snippet:\n%s", err, numberedSnippet(src))
	}
	return syms, diags, err
}

// EnsureAndHover runs HoverAt after ensuring the language server is installed.
func EnsureAndHover(cmd string, args []string, langID, src string, pos protocol.Position) (protocol.Hover, error) {
	return EnsureAndHoverWithRoot(cmd, args, langID, src, pos, "")
}

// EnsureAndHoverWithRoot runs HoverAtWithRoot after ensuring the language server is installed.
func EnsureAndHoverWithRoot(cmd string, args []string, langID, src string, pos protocol.Position, root string) (protocol.Hover, error) {
	if err := EnsureServer(cmd); err != nil {
		return protocol.Hover{}, err
	}
	hov, err := HoverAtWithRoot(cmd, args, langID, src, pos, root)
	if err != nil {
		err = fmt.Errorf("hover failure: %w\n\nsource snippet:\n%s", err, numberedSnippet(src))
	}
	return hov, err
}

// EnsureAndDefinition runs DefinitionAt after ensuring the language server is installed.
func EnsureAndDefinition(cmd string, args []string, langID, src string, pos protocol.Position) ([]protocol.Location, error) {
	return EnsureAndDefinitionWithRoot(cmd, args, langID, src, pos, "")
}

// EnsureAndDefinitionWithRoot runs DefinitionAtWithRoot after ensuring the language server is installed.
func EnsureAndDefinitionWithRoot(cmd string, args []string, langID, src string, pos protocol.Position, root string) ([]protocol.Location, error) {
	if err := EnsureServer(cmd); err != nil {
		return nil, err
	}
	locs, err := DefinitionAtWithRoot(cmd, args, langID, src, pos, root)
	if err != nil {
		err = fmt.Errorf("definition failure: %w\n\nsource snippet:\n%s", err, numberedSnippet(src))
	}
	return locs, err
}

func numberedSnippet(src string) string {
	lines := strings.Split(src, "\n")
	if len(lines) > 10 {
		lines = lines[:10]
	}
	for i, l := range lines {
		lines[i] = fmt.Sprintf("%3d: %s", i+1, l)
	}
	return strings.Join(lines, "\n")
}

func formatDiagnostics(src string, diags []protocol.Diagnostic) string {
	lines := strings.Split(src, "\n")
	var out strings.Builder
	for _, d := range diags {
		start := int(d.Range.Start.Line)
		msg := d.Message
		line := ""
		if start < len(lines) {
			line = strings.TrimSpace(lines[start])
		}
		out.WriteString(fmt.Sprintf("line %d: %s\n  %s\n", start+1, msg, line))
	}
	return strings.TrimSpace(out.String())
}
