package any2mochi

import (
	"fmt"
	protocol "github.com/tliron/glsp/protocol_3_16"
	"strings"
)

// EnsureAndParse runs ParseText after ensuring the language server is installed.
// Any parsing errors are annotated with a short snippet of the source for easier debugging.
func EnsureAndParse(cmd string, args []string, langID, src string) ([]protocol.DocumentSymbol, []protocol.Diagnostic, error) {
	if err := EnsureServer(cmd); err != nil {
		return nil, nil, err
	}
	syms, diags, err := ParseText(cmd, args, langID, src)
	if err != nil {
		err = fmt.Errorf("parse failure: %w\n\nsource snippet:\n%s", err, numberedSnippet(src))
	}
	return syms, diags, err
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
