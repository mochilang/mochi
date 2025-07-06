package erlang

import (
	"fmt"
	"os"
	"strings"
)

func snippet(src string) string {
	lines := strings.Split(src, "\n")
	if len(lines) > 10 {
		lines = lines[:10]
	}
	for i, l := range lines {
		lines[i] = fmt.Sprintf("%3d: %s", i+1, l)
	}
	return strings.Join(lines, "\n")
}

// Convert converts erlang source code to Mochi using the language server.
func Convert(src string) ([]byte, error) {
	funcs, err := parseAST(src)
	if err != nil {
		return nil, err
	}
	var out strings.Builder
	hasMain := false
	for _, f := range funcs {
		if f.Name == "main" {
			hasMain = true
		}
		out.WriteString("// line ")
		out.WriteString(fmt.Sprint(f.Line))
		out.WriteByte('\n')
		out.WriteString("fun ")
		if f.Name == "" {
			out.WriteString("fun")
		} else {
			out.WriteString(f.Name)
		}
		out.WriteByte('(')
		for i, p := range f.Params {
			if i > 0 {
				out.WriteString(", ")
			}
			out.WriteString(p)
		}
		out.WriteByte(')')
		if len(f.Body) == 0 {
			out.WriteString(" {}\n")
		} else {
			out.WriteString(" {\n")
			for _, line := range f.Body {
				if strings.HasPrefix(line, "io:format(") {
					line = "print(" + strings.TrimPrefix(line, "io:format(")
				} else if strings.HasPrefix(line, "io:fwrite(") {
					line = "print(" + strings.TrimPrefix(line, "io:fwrite(")
				}
				out.WriteString("  ")
				out.WriteString(line)
				out.WriteByte('\n')
			}
			out.WriteString("}\n")
		}
	}
	if hasMain {
		out.WriteString("main()\n")
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", snippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertFile reads the erlang file and converts it to Mochi.
func ConvertFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Convert(string(data))
}
