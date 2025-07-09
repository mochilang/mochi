//go:build slow

package swift

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"regexp"
	"strconv"
	"strings"
	"time"

	any2mochi "mochi/archived/tools/any2mochi"
)

// ConvertError provides a detailed error message for Swift conversion.
type ConvertError struct {
	Line   int
	Column int
	Msg    string
	Snip   string
}

func (e *ConvertError) Error() string {
	if e.Line > 0 {
		if e.Column > 0 {
			return fmt.Sprintf("line %d:%d: %s\n%s", e.Line, e.Column, e.Msg, e.Snip)
		}
		return fmt.Sprintf("line %d: %s\n%s", e.Line, e.Msg, e.Snip)
	}
	return fmt.Sprintf("%s\n%s", e.Msg, e.Snip)
}

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
			if it.Name != nil && strings.HasPrefix(it.Name.BaseName.Name, "_") {
				continue
			}
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
		case "struct_decl", "class_decl":
			if it.Name == nil {
				continue
			}
			if strings.HasPrefix(it.Name.BaseName.Name, "_") {
				continue
			}
			out.WriteString("type ")
			out.WriteString(it.Name.BaseName.Name)
			out.WriteString(" {\n")
			for _, m := range it.Members {
				if m.Kind != "var_decl" || m.Name == nil {
					continue
				}
				out.WriteString("  ")
				out.WriteString(m.Name.BaseName.Name)
				if t := interfaceTypeToMochi(m.InterfaceType); t != "" {
					out.WriteString(": ")
					out.WriteString(t)
				}
				out.WriteByte('\n')
			}
			out.WriteString("}\n")
		case "enum_decl":
			if it.Name == nil {
				continue
			}
			if strings.HasPrefix(it.Name.BaseName.Name, "_") {
				continue
			}
			out.WriteString("type ")
			out.WriteString(it.Name.BaseName.Name)
			out.WriteString(" =\n")
			els := gatherEnumElements(it.Members)
			for i, el := range els {
				if i > 0 {
					out.WriteString("  | ")
				} else {
					out.WriteString("  ")
				}
				out.WriteString(el.Name.BaseName.Name)
				if el.Params != nil && len(el.Params.Params) > 0 {
					out.WriteByte('(')
					for j, p := range el.Params.Params {
						if j > 0 {
							out.WriteString(", ")
						}
						out.WriteString(p.Name.BaseName.Name)
						if t := interfaceTypeToMochi(p.InterfaceType); t != "" {
							out.WriteString(": ")
							out.WriteString(t)
						}
					}
					out.WriteByte(')')
				}
				out.WriteByte('\n')
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
	Kind          string      `json:"_kind"`
	Name          *declName   `json:"name,omitempty"`
	Params        *paramList  `json:"params,omitempty"`
	Body          *body       `json:"body,omitempty"`
	Range         offsetRange `json:"range"`
	Result        string      `json:"result,omitempty"`
	InterfaceType string      `json:"interface_type,omitempty"`
	ThrownType    string      `json:"thrown_type,omitempty"`
	Access        string      `json:"access,omitempty"`
	Members       []item      `json:"members,omitempty"`
	Elements      []item      `json:"elements,omitempty"`
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
	cmd.Stderr = &out
	if err := cmd.Run(); err != nil {
		line, col := 0, 0
		msg := err.Error()
		re := regexp.MustCompile(`:(\d+):(\d+): error: (.*)`)
		if m := re.FindStringSubmatch(out.String()); len(m) == 4 {
			line, _ = strconv.Atoi(m[1])
			col, _ = strconv.Atoi(m[2])
			msg = m[3]
		}
		return file{}, &ConvertError{Line: line, Column: col, Msg: msg, Snip: snippetAround(src, line, col)}
	}
	data := out.Bytes()
	if idx := bytes.IndexByte(data, '{'); idx > 0 {
		data = data[idx:]
	}
	dec := json.NewDecoder(bytes.NewReader(data))
	var f file
	if err := dec.Decode(&f); err != nil {
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
		for _, part := range strings.Split(line, ";") {
			l := strings.TrimSpace(part)
			if l == "" {
				continue
			}
			l = strings.TrimSpace(l)
			l = strings.TrimSuffix(l, ";")
			l = rewriteStructLiteral(l)
			l = strings.ReplaceAll(l, "_append(", "append(")
			l = strings.ReplaceAll(l, "_values(", "values(")
			l = strings.ReplaceAll(l, "_exists(", "exists(")
			l = strings.ReplaceAll(l, "_sliceString(", "substring(")
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
			case strings.HasPrefix(l, "while ") && strings.HasSuffix(l, "{"):
				cond := strings.TrimSpace(strings.TrimSuffix(l[len("while "):], "{"))
				out = append(out, strings.Repeat("  ", indent)+"while "+cond+" {")
				indent++
			case strings.HasPrefix(l, "return "):
				expr := rewriteStructLiteral(strings.TrimSpace(l[len("return "):]))
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
					expr := rewriteStructLiteral(strings.TrimSpace(parts[1]))
					out = append(out, strings.Repeat("  ", indent)+"let "+name+" = "+expr)
				}
			case l == "break":
				out = append(out, strings.Repeat("  ", indent)+"break")
			case l == "continue":
				out = append(out, strings.Repeat("  ", indent)+"continue")
			default:
				out = append(out, strings.Repeat("  ", indent)+l)
			}
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

var structLitRE = regexp.MustCompile(`^([A-Z][A-Za-z0-9_]*)\((.*)\)$`)

func rewriteStructLiteral(expr string) string {
	m := structLitRE.FindStringSubmatch(expr)
	if len(m) != 3 || !strings.Contains(m[2], ":") {
		return expr
	}
	name := m[1]
	parts := strings.Split(m[2], ",")
	var fields []string
	for _, p := range parts {
		kv := strings.SplitN(strings.TrimSpace(p), ":", 2)
		if len(kv) != 2 {
			return expr
		}
		fields = append(fields, fmt.Sprintf("%s: %s", strings.TrimSpace(kv[0]), strings.TrimSpace(kv[1])))
	}
	return name + " { " + strings.Join(fields, ", ") + " }"
}

func gatherEnumElements(ms []item) []item {
	var out []item
	for _, m := range ms {
		switch m.Kind {
		case "enum_case_decl":
			out = append(out, m.Elements...)
		case "enum_element_decl":
			out = append(out, m)
		}
	}
	return out
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

func snippetAround(src string, line, col int) string {
	lines := strings.Split(src, "\n")
	if line <= 0 || line > len(lines) {
		return snippet(src)
	}
	start := line - 2
	if start < 0 {
		start = 0
	}
	end := line + 2
	if end > len(lines) {
		end = len(lines)
	}
	var out strings.Builder
	for i := start; i < end; i++ {
		fmt.Fprintf(&out, "%4d| %s\n", i+1, lines[i])
		if i == line-1 {
			caretPos := len(lines[i])
			if col > 0 && col-1 < caretPos {
				caretPos = col - 1
			}
			out.WriteString("    | " + strings.Repeat(" ", caretPos) + "^\n")
		}
	}
	return strings.TrimRight(out.String(), "\n")
}

// ConvertFile reads the Swift file and converts it to Mochi.
func ConvertFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Convert(string(data))
}
