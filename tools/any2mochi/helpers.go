package any2mochi

import (
	"encoding/json"
	"fmt"
	"os/exec"
	"regexp"
	"strconv"
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
func NumberedSnippet(src string) string {
	lines := strings.Split(src, "\n")
	if len(lines) > 10 {
		lines = lines[:10]
	}
	for i, l := range lines {
		lines[i] = fmt.Sprintf("%3d: %s", i+1, l)
	}
	return strings.Join(lines, "\n")
}

// ErrorWithSnippet attaches numbered source context around the line mentioned in err, if any.
func ErrorWithSnippet(err error, src string) error {
	if err == nil || src == "" {
		return err
	}
	line := -1
	re := regexp.MustCompile(`(?i)line\s+(\d+)`)
	if m := re.FindStringSubmatch(err.Error()); m != nil {
		if n, err2 := strconv.Atoi(m[1]); err2 == nil {
			line = n
		}
	}
	var snippet string
	if line > 0 {
		snippet = numberedContext(src, line, 2)
	} else {
		snippet = NumberedSnippet(src)
	}
	return fmt.Errorf("%v\n\n%s", err, snippet)
}

func numberedContext(src string, line, ctx int) string {
	lines := strings.Split(src, "\n")
	start := line - ctx - 1
	if start < 0 {
		start = 0
	}
	end := line + ctx - 1
	if end >= len(lines) {
		end = len(lines) - 1
	}
	var out []string
	for i := start; i <= end && i < len(lines); i++ {
		prefix := "   "
		if i+1 == line {
			prefix = "-> "
		}
		out = append(out, fmt.Sprintf("%s%3d: %s", prefix, i+1, lines[i]))
	}
	return strings.Join(out, "\n")
}

// formatDiagnostics returns a human readable diagnostics string.
func formatDiagnostics(src string, diags []Diagnostic) string {
	lines := strings.Split(src, "\n")
	var out strings.Builder
	for _, d := range diags {
		start := int(d.Range.Start.Line)
		col := int(d.Range.Start.Character)
		msg := d.Message
		from := start - 2
		if from < 0 {
			from = 0
		}
		to := start + 2
		if to >= len(lines) {
			to = len(lines) - 1
		}
		out.WriteString(fmt.Sprintf("line %d:%d: %s\n", start+1, col+1, msg))
		for i := from; i <= to; i++ {
			out.WriteString(fmt.Sprintf("%4d| %s\n", i+1, lines[i]))
			if i == start {
				out.WriteString("     " + strings.Repeat(" ", col) + "^\n")
			}
		}
		out.WriteByte('\n')
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

// extractRangeText returns the substring covered by r.
func extractRangeText(src string, r Range) string {
	lines := strings.Split(src, "\n")
	var out strings.Builder
	for i := int(r.Start.Line); i <= int(r.End.Line) && i < len(lines); i++ {
		line := lines[i]
		if i == int(r.Start.Line) && int(r.Start.Character) < len(line) {
			line = line[int(r.Start.Character):]
		}
		if i == int(r.End.Line) && int(r.End.Character) <= len(line) {
			line = line[:int(r.End.Character)]
		}
		out.WriteString(line)
		if i != int(r.End.Line) {
			out.WriteByte('\n')
		}
	}
	return out.String()
}
