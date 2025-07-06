package any2mochi

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os/exec"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ParseText runs cmdName with args, feeding src on stdin. The command
// is expected to output a JSON object containing "symbols" and
// optional "diagnostics" fields.
func ParseText(cmdName string, args []string, langID, src string) ([]protocol.DocumentSymbol, []protocol.Diagnostic, error) {
	cmd := exec.Command(cmdName, args...)
	cmd.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Run(); err != nil {
		return nil, nil, err
	}
	var res struct {
		Symbols     []protocol.DocumentSymbol `json:"symbols"`
		Diagnostics []protocol.Diagnostic     `json:"diagnostics"`
	}
	if err := json.Unmarshal(out.Bytes(), &res); err != nil {
		return nil, nil, err
	}
	return res.Symbols, res.Diagnostics, nil
}

// ParseTextWithRoot is a compatibility wrapper that ignores the root parameter.
func ParseTextWithRoot(cmdName string, args []string, langID, src, root string) ([]protocol.DocumentSymbol, []protocol.Diagnostic, error) {
	return ParseText(cmdName, args, langID, src)
}

// EnsureServer is a stub kept for backwards compatibility.
func EnsureServer(name string) error { return nil }

// EnsureAndParse is equivalent to ParseText with no installation step.
func EnsureAndParse(cmd string, args []string, langID, src string) ([]protocol.DocumentSymbol, []protocol.Diagnostic, error) {
	return ParseText(cmd, args, langID, src)
}

// EnsureAndParseWithRoot is equivalent to ParseTextWithRoot.
func EnsureAndParseWithRoot(cmd string, args []string, langID, src, root string) ([]protocol.DocumentSymbol, []protocol.Diagnostic, error) {
	return ParseTextWithRoot(cmd, args, langID, src, root)
}

// EnsureAndHover returns an empty hover result.
func EnsureAndHover(cmd string, args []string, langID, src string, pos protocol.Position) (protocol.Hover, error) {
	return protocol.Hover{}, nil
}

// EnsureAndHoverWithRoot is identical to EnsureAndHover.
func EnsureAndHoverWithRoot(cmd string, args []string, langID, src string, pos protocol.Position, root string) (protocol.Hover, error) {
	return EnsureAndHover(cmd, args, langID, src, pos)
}

// EnsureAndDefinition returns no locations.
func EnsureAndDefinition(cmd string, args []string, langID, src string, pos protocol.Position) ([]protocol.Location, error) {
	return nil, nil
}

// EnsureAndDefinitionWithRoot is identical to EnsureAndDefinition.
func EnsureAndDefinitionWithRoot(cmd string, args []string, langID, src string, pos protocol.Position, root string) ([]protocol.Location, error) {
	return EnsureAndDefinition(cmd, args, langID, src, pos)
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

func hoverString(h protocol.Hover) string {
	switch v := h.Contents.(type) {
	case protocol.MarkupContent:
		return v.Value
	case protocol.MarkedString:
		return markedStringValue(v)
	case []protocol.MarkedString:
		var parts []string
		for _, m := range v {
			parts = append(parts, markedStringValue(m))
		}
		return strings.Join(parts, "\n")
	case string:
		return v
	default:
		return fmt.Sprintf("%v", v)
	}
}

func markedStringValue(m protocol.MarkedString) string {
	b, err := json.Marshal(m)
	if err != nil {
		return fmt.Sprintf("%v", m)
	}
	var s string
	if err := json.Unmarshal(b, &s); err == nil {
		return s
	}
	var obj struct {
		Language string `json:"language"`
		Value    string `json:"value"`
	}
	if err := json.Unmarshal(b, &obj); err == nil {
		return obj.Value
	}
	return string(b)
}
