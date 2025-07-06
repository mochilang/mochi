package any2mochi

import (
	"fmt"
	"os"
	"strings"
)

// ConvertPho converts Pho source code to Mochi using simple regex parsing.
func ConvertPho(src string) ([]byte, error) {
	syms := parsePho(src)
	if len(syms) == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	var out strings.Builder
	for _, s := range syms {
		out.WriteString("fun ")
		out.WriteString(s.Name)
		params := strings.TrimPrefix(*s.Detail, "(")
		params = strings.TrimSuffix(params, ")")
		out.WriteString("(")
		out.WriteString(strings.TrimSpace(params))
		out.WriteString(")")
		body := extractPhoBody(src, s)
		if len(body) == 0 {
			out.WriteString(" {}\n")
		} else {
			out.WriteString(" {\n")
			for _, line := range body {
				out.WriteString("  ")
				out.WriteString(line)
				out.WriteByte('\n')
			}
			out.WriteString("}\n")
		}
	}
	return []byte(out.String()), nil
}

// ConvertPhoFile reads the pho file and converts it to Mochi.
func ConvertPhoFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertPho(string(data))
}
