package any2mochi

import (
	"encoding/json"
	"fmt"
	"os/exec"
	"strings"
)

// UseLSP controls whether language servers are used for conversion.
var UseLSP = true

// EnsureServer checks that cmd is available on the system PATH.
// It returns an error if the executable is not found.
func EnsureServer(cmd string) error {
	if cmd == "" {
		return fmt.Errorf("missing server command")
	}
	_, err := exec.LookPath(cmd)
	return err
}

// numberedSnippet formats the first few lines of src with line numbers.
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

// formatDiagnostics returns a human readable diagnostics string.
func formatDiagnostics(src string, diags []Diagnostic) string {
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

// hoverString extracts a plain text string from a Hover response.
func hoverString(h Hover) string {
	switch v := h.Contents.(type) {
	case MarkupContent:
		return v.Value
	case MarkedString:
		b, err := json.Marshal(v)
		if err == nil {
			var ms MarkedStringStruct
			if err := json.Unmarshal(b, &ms); err == nil {
				if ms.Value != "" {
					return ms.Value
				}
			}
			var s string
			if err := json.Unmarshal(b, &s); err == nil {
				return s
			}
		}
		return ""
	case []MarkedString:
		parts := make([]string, 0, len(v))
		for _, m := range v {
			parts = append(parts, hoverString(Hover{Contents: m}))
		}
		return strings.Join(parts, "\n")
	case string:
		return v
	default:
		return fmt.Sprint(v)
	}
}
