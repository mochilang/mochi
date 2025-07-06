package any2mochi

import (
	"fmt"
	"os"
	"strings"
)

// ConvertPhp parses PHP source using ParsePhp and emits minimal Mochi stubs.
func ConvertPhp(src string) ([]byte, error) {
	prog, err := ParsePhp(src)
	if err != nil {
		return nil, err
	}
	var out strings.Builder
	for _, c := range prog.Classes {
		out.WriteString("type ")
		out.WriteString(c.Name)
		if len(c.Fields) == 0 && len(c.Methods) == 0 {
			out.WriteString(" {}\n")
		} else {
			out.WriteString(" {\n")
			for _, f := range c.Fields {
				out.WriteString("  ")
				out.WriteString(f)
				out.WriteString(": any\n")
			}
			for _, m := range c.Methods {
				out.WriteString("  fun ")
				out.WriteString(m.Name)
				out.WriteByte('(')
				for i, p := range m.Params {
					if i > 0 {
						out.WriteString(", ")
					}
					out.WriteString(p)
				}
				out.WriteString(") {}\n")
			}
			out.WriteString("}\n")
		}
	}
	for _, f := range prog.Functions {
		out.WriteString("fun ")
		out.WriteString(f.Name)
		out.WriteByte('(')
		for i, p := range f.Params {
			if i > 0 {
				out.WriteString(", ")
			}
			out.WriteString(p)
		}
		out.WriteString(") {}\n")
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertPhpFile reads the php file and converts it to Mochi.
func ConvertPhpFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertPhp(string(data))
}
