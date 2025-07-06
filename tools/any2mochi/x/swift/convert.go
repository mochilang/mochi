package swift

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"time"

	any2mochi "mochi/tools/any2mochi"
)

// Convert converts Swift source code to Mochi. It invokes the Swift
// compiler to dump the AST as JSON and then walks a minimal subset of the
// structure to produce runnable Mochi code.
func Convert(src string) ([]byte, error) {
	ast, err := parse(src)
	if err != nil {
		return nil, err
	}
	var out strings.Builder
	for _, it := range ast.Items {
		switch it.Kind {
		case "func_decl":
			out.WriteString("fun ")
			if it.Name != nil {
				out.WriteString(it.Name.BaseName.Name)
			}
			out.WriteByte('(')
			if it.Params != nil {
				for i, p := range it.Params.Params {
					if i > 0 {
						out.WriteString(", ")
					}
					out.WriteString(p.Name.BaseName.Name)
					if t := interfaceTypeToMochi(p.InterfaceType); t != "" {
						out.WriteString(": ")
						out.WriteString(t)
					}
				}
			}
			out.WriteByte(')')
			if t := interfaceTypeToMochi(it.Result); t != "" {
				out.WriteString(": ")
				out.WriteString(t)
			}
			if it.Body != nil {
				body := bodyFromRange(src, it.Body.Range)
				if len(body) == 0 {
					out.WriteString(" {}\n")
				} else {
					out.WriteString(" {\n")
					for _, st := range body {
						out.WriteString("  ")
						out.WriteString(st)
						out.WriteByte('\n')
					}
					out.WriteString("}\n")
				}
			} else {
				out.WriteString(" {}\n")
			}
		case "top_level_code_decl":
			code := extractRange(src, it.Range)
			for _, st := range parseStatementsIndent(code, 0) {
				out.WriteString(st)
				out.WriteByte('\n')
			}
		}
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", snippet(src))
	}
	return []byte(out.String()), nil
}

type file struct {
	Items []item `json:"items"`
}

type item struct {
	Kind   string      `json:"_kind"`
	Name   *declName   `json:"name,omitempty"`
	Params *paramList  `json:"params,omitempty"`
	Body   *body       `json:"body,omitempty"`
	Range  offsetRange `json:"range"`
	Result string      `json:"result,omitempty"`
}

type declName struct {
	BaseName baseName `json:"base_name"`
}

type baseName struct {
	Name string `json:"name"`
}

type paramList struct {
	Params []param `json:"params"`
}

type param struct {
	Name          declName `json:"name"`
	InterfaceType string   `json:"interface_type,omitempty"`
}

type body struct {
	Range offsetRange `json:"range"`
}

type offsetRange struct {
	Start int `json:"start"`
	End   int `json:"end"`
}

// parse invokes swiftc to dump the AST as JSON.
func parse(src string) (file, error) {
	tmp, err := os.CreateTemp("", "swift-src-*.swift")
	if err != nil {
		return file{}, err
	}
	defer os.Remove(tmp.Name())
	if _, err := tmp.WriteString(src); err != nil {
		tmp.Close()
		return file{}, err
	}
	tmp.Close()

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, "swiftc", "-dump-ast", "-dump-ast-format", "json", tmp.Name())
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Run(); err != nil {
		return file{}, err
	}
	var f file
	if err := json.Unmarshal(out.Bytes(), &f); err != nil {
		return file{}, err
	}
	return f, nil
}

func extractRange(src string, r offsetRange) string {
	if r.Start < 0 {
		r.Start = 0
	}
	end := r.End
	if end < r.Start {
		end = r.Start
	}
	if end+1 > len(src) {
		end = len(src) - 1
	}
	return src[r.Start : end+1]
}

func bodyFromRange(src string, r offsetRange) []string {
	text := extractRange(src, r)
	start := strings.Index(text, "{")
	end := strings.LastIndex(text, "}")
	if start == -1 || end == -1 || end <= start {
		return nil
	}
	return parseStatementsIndent(text[start+1:end], 1)
}

func interfaceTypeToMochi(t string) string {
	if strings.HasPrefix(t, "$sS") && strings.HasSuffix(t, "D") {
		mid := strings.TrimSuffix(strings.TrimPrefix(t, "$sS"), "D")
		switch mid {
		case "i":
			return "int"
		case "d":
			return "float"
		case "b":
			return "bool"
		case "S":
			return "string"
		}
	}
	return ""
}

func parseStatements(body string) []string {
	return parseStatementsIndent(body, 1)
}

func parseStatementsIndent(body string, indent int) []string {
	lines := strings.Split(body, "\n")
	var out []string
	for _, line := range lines {
		l := strings.TrimSpace(line)
		if l == "" {
			continue
		}
		l = strings.TrimSuffix(l, ";")
		switch {
		case l == "}":
			if indent > 0 {
				indent--
			}
			out = append(out, strings.Repeat("  ", indent)+"}")
		case strings.HasPrefix(l, "if ") && strings.HasSuffix(l, "{"):
			cond := strings.TrimSpace(strings.TrimSuffix(l[3:], "{"))
			out = append(out, strings.Repeat("  ", indent)+"if "+cond+" {")
			indent++
		case l == "else {":
			if indent > 0 {
				indent--
			}
			out = append(out, strings.Repeat("  ", indent)+"else {")
			indent++
		case strings.HasPrefix(l, "for ") && strings.HasSuffix(l, "{"):
			head := strings.TrimSpace(strings.TrimSuffix(l[4:], "{"))
			out = append(out, strings.Repeat("  ", indent)+"for "+head+" {")
			indent++
		case strings.HasPrefix(l, "return "):
			expr := strings.TrimSpace(l[len("return "):])
			out = append(out, strings.Repeat("  ", indent)+"return "+expr)
		case strings.HasPrefix(l, "let ") || strings.HasPrefix(l, "var "):
			decl := strings.TrimPrefix(l, "let ")
			if decl == l {
				decl = strings.TrimPrefix(l, "var ")
			}
			parts := strings.SplitN(decl, "=", 2)
			if len(parts) == 2 {
				name := strings.TrimSpace(parts[0])
				if colon := strings.Index(name, ":"); colon != -1 {
					name = strings.TrimSpace(name[:colon])
				}
				expr := strings.TrimSpace(parts[1])
				out = append(out, strings.Repeat("  ", indent)+"let "+name+" = "+expr)
			}
		default:
			out = append(out, strings.Repeat("  ", indent)+l)
		}
	}
	return out
}

func extractRangeText(src string, r any2mochi.Range) string {
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

// ConvertFile reads the Swift file and converts it to Mochi.
func ConvertFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Convert(string(data))
}
