package mfmt

import (
	"bufio"
	"os"
	"strings"
)

// Format formats Mochi source code using a very small
// set of heuristics for consistent indentation and braces.
// It is not a full pretty printer but provides a stable style.
func Format(src string) string {
	var out []string
	indent := 0
	scanner := bufio.NewScanner(strings.NewReader(src))
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			out = append(out, "")
			continue
		}
		if strings.HasPrefix(line, "}") || strings.HasPrefix(line, "]") || strings.HasPrefix(line, ")") {
			if indent > 0 {
				indent--
			}
		}
		line = strings.ReplaceAll(line, "}else", "} else")
		line = strings.ReplaceAll(line, "else{", "else {")
		line = strings.ReplaceAll(line, "){", ") {")
		if strings.Contains(line, "{") {
			line = strings.ReplaceAll(line, "{", " {")
			line = strings.TrimPrefix(line, " {")
			line = strings.ReplaceAll(line, "  {", " {")
		}
		out = append(out, strings.Repeat("  ", indent)+line)
		if strings.HasSuffix(line, "{") || strings.HasSuffix(line, "[") || strings.HasSuffix(line, "(") {
			indent++
		}
	}
	return strings.Join(out, "\n") + "\n"
}

// FormatFile reads a file and returns the formatted source.
func FormatFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	formatted := Format(string(data))
	return []byte(formatted), nil
}
