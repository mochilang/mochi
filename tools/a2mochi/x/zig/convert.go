package zig

import (
	"fmt"
	"os"
	"regexp"
	"strings"

	"mochi/ast"
	"mochi/parser"
	"mochi/transpiler/meta"
)

// ConvertSource converts Zig source code to Mochi source. The current
// implementation is a stub and always returns an error.
func ConvertSource(src string) (string, error) {
	var out strings.Builder

	// add header comment with version and time
	out.Write(meta.Header("//"))

	// include the original Zig code as a block comment
	out.WriteString("/*\n")
	out.WriteString(src)
	if !strings.HasSuffix(src, "\n") {
		out.WriteByte('\n')
	}
	out.WriteString("*/\n")

	code, err := translate(src)
	if err != nil {
		return "", err
	}
	out.WriteString(code)
	if !strings.HasSuffix(code, "\n") {
		out.WriteByte('\n')
	}
	return out.String(), nil
}

// Convert converts Zig source code into a Mochi AST node. This is currently
// unimplemented and returns an error.
func Convert(src string) (*ast.Node, error) {
	code, err := ConvertSource(src)
	if err != nil {
		return nil, err
	}
	prog, err := parser.ParseString(code)
	if err != nil {
		return nil, err
	}
	return ast.FromProgram(prog), nil
}

// ConvertFile reads the given file and calls Convert on its contents.
func ConvertFile(path string) (*ast.Node, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Convert(string(data))
}

// ConvertFileSource reads the file and returns the Mochi source string.
func ConvertFileSource(path string) (string, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return "", err
	}
	return ConvertSource(string(data))
}

// ----- internal helpers -----

var headerRE = regexp.MustCompile(`^(?:pub\s+)?fn\s+([A-Za-z_][A-Za-z0-9_]*)\s*\(([^)]*)\)\s*([^\{]*)\{`)

func mapType(t string) string {
	switch strings.TrimSpace(t) {
	case "i64", "i32", "u64", "u32":
		return "int"
	case "f64", "f32":
		return "float"
	case "void", "":
		return ""
	default:
		return "any"
	}
}

func convertParams(params string) string {
	parts := strings.Split(strings.TrimSpace(params), ",")
	var out []string
	for _, p := range parts {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		fields := strings.SplitN(p, ":", 2)
		name := strings.TrimSpace(fields[0])
		typ := ""
		if len(fields) == 2 {
			typ = mapType(fields[1])
		}
		if typ != "" {
			out = append(out, fmt.Sprintf("%s: %s", name, typ))
		} else {
			out = append(out, name)
		}
	}
	return strings.Join(out, ", ")
}

func convertFunction(lines []string) string {
	first := strings.TrimSpace(lines[0])
	m := headerRE.FindStringSubmatch(first)
	if m == nil {
		return ""
	}
	name := m[1]
	params := convertParams(m[2])
	ret := mapType(strings.TrimSpace(m[3]))

	var b strings.Builder
	b.WriteString("fun ")
	b.WriteString(name)
	b.WriteByte('(')
	b.WriteString(params)
	b.WriteByte(')')
	if ret != "" {
		b.WriteString(": ")
		b.WriteString(ret)
	}
	b.WriteString(" {\n")

	for i := 1; i < len(lines); i++ {
		t := strings.TrimSpace(lines[i])
		if i == len(lines)-1 && t == "}" {
			break
		}
		t = strings.TrimSuffix(t, ";")
		if strings.HasPrefix(t, "if (") && strings.HasSuffix(t, ") {") {
			cond := strings.TrimSuffix(strings.TrimPrefix(t, "if ("), ") {")
			b.WriteString("  if " + cond + " {\n")
			continue
		}
		if t == "}" {
			b.WriteString("  }\n")
			continue
		}
		b.WriteString("  ")
		b.WriteString(t)
		b.WriteByte('\n')
	}
	b.WriteString("}\n")
	return b.String()
}

var printRE = regexp.MustCompile(`print\("\{[^}]+\}\\n",\s*\.\{(.*)\}\);`)

func extractPrint(line string) string {
	m := printRE.FindStringSubmatch(strings.TrimSpace(line))
	if len(m) == 2 {
		return strings.TrimSpace(m[1])
	}
	return ""
}

func translate(src string) (string, error) {
	lines := strings.Split(src, "\n")
	var out strings.Builder
	for i := 0; i < len(lines); i++ {
		line := strings.TrimSpace(lines[i])
		if line == "" || strings.HasPrefix(line, "//") || strings.HasPrefix(line, "const ") {
			continue
		}
		if strings.HasPrefix(line, "pub fn main") || strings.HasPrefix(line, "fn main") {
			brace := strings.Count(line, "{") - strings.Count(line, "}")
			for brace > 0 && i+1 < len(lines) {
				i++
				l := lines[i]
				brace += strings.Count(l, "{") - strings.Count(l, "}")
				if p := extractPrint(l); p != "" {
					out.WriteString("print(" + p + ")\n")
				}
			}
			continue
		}
		if strings.HasPrefix(line, "fn ") || strings.HasPrefix(line, "pub fn ") {
			brace := strings.Count(line, "{") - strings.Count(line, "}")
			fnLines := []string{line}
			for brace > 0 && i+1 < len(lines) {
				i++
				l := lines[i]
				fnLines = append(fnLines, l)
				brace += strings.Count(l, "{") - strings.Count(l, "}")
			}
			out.WriteString(convertFunction(fnLines))
			continue
		}
	}
	res := out.String()
	if strings.TrimSpace(res) == "" {
		return "", fmt.Errorf("no convertible symbols found")
	}
	return res, nil
}
