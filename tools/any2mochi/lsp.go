package any2mochi

import (
	"encoding/json"
	"fmt"
	protocol "github.com/tliron/glsp/protocol_3_16"
	"strings"
)

// ConvertWithServer converts source code using the provided language server configuration.
func ConvertWithServer(cmd string, args []string, langID, src string) ([]byte, error) {
	syms, diags, err := ParseAndEnsure(cmd, args, langID, src)
	if err != nil {
		return nil, fmt.Errorf("convert failure: %w\n\nsource snippet:\n%s", err, numberedSnippet(src))
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	for _, s := range syms {
		if s.Kind != protocol.SymbolKindFunction {
			continue
		}
		out.WriteString("fun ")
		out.WriteString(s.Name)
		out.WriteString("() {}\n")
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertWithServerJSON is like ConvertWithServer but also returns the parsed
// symbols encoded as pretty JSON.
func ConvertWithServerJSON(cmd string, args []string, langID, src string) ([]byte, []byte, error) {
	syms, diags, err := ParseAndEnsure(cmd, args, langID, src)
	if err != nil {
		return nil, nil, fmt.Errorf("convert failure: %w\n\nsource snippet:\n%s", err, numberedSnippet(src))
	}
	if len(diags) > 0 {
		return nil, nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	for _, s := range syms {
		if s.Kind != protocol.SymbolKindFunction {
			continue
		}
		out.WriteString("fun ")
		out.WriteString(s.Name)
		out.WriteString("() {}\n")
	}
	if out.Len() == 0 {
		return nil, nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	js, _ := json.MarshalIndent(syms, "", "  ")
	return []byte(out.String()), js, nil
}

// ParseAndEnsure runs ParseText after ensuring the language server is installed.
func ParseAndEnsure(cmd string, args []string, langID, src string) ([]protocol.DocumentSymbol, []protocol.Diagnostic, error) {
	if err := EnsureServer(cmd); err != nil {
		return nil, nil, err
	}
	return ParseText(cmd, args, langID, src)
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
