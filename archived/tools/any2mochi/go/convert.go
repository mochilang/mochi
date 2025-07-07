package golang

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/printer"
	"go/scanner"
	"go/token"
	"os"
	"strings"
	"unicode"
)

// ConvertError provides a detailed error suitable for golden tests.
type ConvertError struct {
	Line   int
	Column int
	Msg    string
	Snip   string
}

var mochiReserved = map[string]bool{
	"let": true, "fun": true, "if": true, "else": true, "for": true,
	"return": true, "true": true, "false": true, "stream": true,
	"agent": true, "test": true, "expect": true, "on": true,
	"intent": true, "import": true, "extern": true, "var": true,
	"break": true, "continue": true, "while": true, "in": true,
	"generate": true, "match": true, "fetch": true, "load": true,
	"save": true, "package": true, "export": true, "fact": true,
	"rule": true, "all": true, "null": true,
}

func sanitizeName(name string) string {
	if name == "" {
		return ""
	}
	var b strings.Builder
	for i, r := range name {
		if unicode.IsLetter(r) || r == '_' || (unicode.IsDigit(r) && i > 0) {
			b.WriteRune(r)
		} else {
			b.WriteRune('_')
		}
	}
	out := b.String()
	if out == "" || !(unicode.IsLetter(rune(out[0])) || out[0] == '_') {
		out = "_" + out
	}
	if mochiReserved[out] {
		out = "_" + out
	}
	return out
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

// Convert converts Go source code to Mochi without relying on a
// language server. It parses the source using the standard library
// and emits minimal Mochi stubs.
func Convert(src string) ([]byte, error) {
	ast, err := ParseAST(src)
	if err != nil {
		return nil, formatParseError(src, err)
	}
	out := ConvertAST(ast)
	if len(out) == 0 {
		return nil, &ConvertError{Msg: "no convertible symbols found", Snip: numberedSnippet(src)}
	}
	return out, nil
}

// ConvertFile reads the Go file at path and converts it to Mochi.
func ConvertFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	out, err := Convert(string(data))
	if err != nil {
		return nil, formatError(path, string(data), err)
	}
	return out, nil
}

func writeFunc(out *strings.Builder, fset *token.FileSet, fn *ast.FuncDecl) {
	if fn.Name == nil {
		return
	}
	name := sanitizeName(fn.Name.Name)
	if fn.Recv != nil && len(fn.Recv.List) > 0 {
		recv := exprString(fset, fn.Recv.List[0].Type)
		name = sanitizeName(recv) + "." + name
	}
	out.WriteString("fun ")
	out.WriteString(name)
	out.WriteByte('(')
	first := true
	if fn.Type.Params != nil {
		for _, p := range fn.Type.Params.List {
			typ := exprString(fset, p.Type)
			if len(p.Names) == 0 {
				if !first {
					out.WriteString(", ")
				}
				out.WriteString("_")
				if typ != "" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				first = false
				continue
			}
			for _, n := range p.Names {
				if !first {
					out.WriteString(", ")
				}
				out.WriteString(sanitizeName(n.Name))
				if typ != "" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				first = false
			}
		}
	}
	out.WriteByte(')')
	if fn.Type.Results != nil {
		var rets []string
		for _, r := range fn.Type.Results.List {
			typ := exprString(fset, r.Type)
			if typ == "" || typ == "void" {
				continue
			}
			n := len(r.Names)
			if n == 0 {
				n = 1
			}
			for i := 0; i < n; i++ {
				rets = append(rets, typ)
			}
		}
		if len(rets) == 1 {
			out.WriteString(": ")
			out.WriteString(rets[0])
		} else if len(rets) > 1 {
			out.WriteString(": (")
			out.WriteString(strings.Join(rets, ", "))
			out.WriteString(")")
		}
	}
	out.WriteString(" {}\n")
}

func writeGenDecl(out *strings.Builder, fset *token.FileSet, d *ast.GenDecl) {
	switch d.Tok {
	case token.TYPE:
		for _, spec := range d.Specs {
			ts, ok := spec.(*ast.TypeSpec)
			if !ok {
				continue
			}
			if st, ok := ts.Type.(*ast.StructType); ok {
				out.WriteString("type ")
				out.WriteString(sanitizeName(ts.Name.Name))
				out.WriteString(" {\n")
				for _, f := range st.Fields.List {
					typ := exprString(fset, f.Type)
					if len(f.Names) == 0 {
						out.WriteString("  ")
						out.WriteString(typ)
						out.WriteByte('\n')
						continue
					}
					for _, n := range f.Names {
						out.WriteString("  ")
						out.WriteString(sanitizeName(n.Name))
						if typ != "" {
							out.WriteString(": ")
							out.WriteString(typ)
						}
						out.WriteByte('\n')
					}
				}
				out.WriteString("}\n")
			}
		}
	case token.VAR, token.CONST:
		for _, spec := range d.Specs {
			vs, ok := spec.(*ast.ValueSpec)
			if !ok {
				continue
			}
			typ := ""
			if vs.Type != nil {
				typ = exprString(fset, vs.Type)
			}
			for _, n := range vs.Names {
				out.WriteString("let ")
				out.WriteString(sanitizeName(n.Name))
				if typ != "" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				out.WriteByte('\n')
			}
		}
	}
}

func exprString(fset *token.FileSet, e ast.Expr) string {
	var b bytes.Buffer
	printer.Fprint(&b, fset, e)
	return simplifyType(b.String())
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

func simplifyType(t string) string {
	t = strings.TrimSpace(t)
	if strings.HasPrefix(t, "*") {
		return simplifyType(strings.TrimPrefix(t, "*"))
	}
	if strings.HasPrefix(t, "[]") {
		return "list<" + simplifyType(t[2:]) + ">"
	}
	if strings.HasPrefix(t, "map[") {
		end := strings.Index(t, "]")
		if end > 0 {
			key := simplifyType(t[4:end])
			val := simplifyType(t[end+1:])
			return fmt.Sprintf("map<%s, %s>", key, val)
		}
	}
	if strings.HasPrefix(t, "func(") {
		end := strings.LastIndex(t, ")")
		if end > 0 {
			paramsPart := t[len("func("):end]
			rest := strings.TrimSpace(t[end+1:])
			var params []string
			if strings.TrimSpace(paramsPart) != "" {
				for _, p := range strings.Split(paramsPart, ",") {
					p = strings.TrimSpace(p)
					if strings.HasPrefix(p, "...") {
						t := simplifyType(strings.TrimSpace(p[3:]))
						params = append(params, "list<"+t+">")
					} else {
						params = append(params, simplifyType(p))
					}
				}
			}
			paramStr := strings.Join(params, ", ")
			if rest != "" {
				ret := strings.Trim(rest, "() ")
				ret = simplifyType(ret)
				if strings.Contains(rest, ",") {
					ret = "(" + ret + ")"
				}
				return fmt.Sprintf("fun(%s): %s", paramStr, ret)
			}
			return fmt.Sprintf("fun(%s)", paramStr)
		}
	}
	if t == "interface{}" {
		return "any"
	}
	// Replace package qualifiers in type names with underscores so that the
	// resulting identifier is a valid Mochi type reference. This avoids
	// parse errors for types like "time.Duration" which are emitted by the
	// Go compiler for helper functions.
	if strings.ContainsAny(t, "./") {
		t = strings.NewReplacer(".", "_", "/", "_").Replace(t)
	}
	return t
}

func formatParseError(src string, err error) error {
	if el, ok := err.(scanner.ErrorList); ok && len(el) > 0 {
		e := el[0]
		pos := e.Pos
		line := int(pos.Line)
		col := int(pos.Column)
		return &ConvertError{Line: line, Column: col, Msg: e.Msg, Snip: arrowSnippet(src, line, col)}
	}
	return err
}

func arrowSnippet(src string, line, col int) string {
	lines := strings.Split(src, "\n")
	start := line - 3
	if start < 0 {
		start = 0
	}
	end := line + 2
	if end > len(lines) {
		end = len(lines)
	}
	var b strings.Builder
	for i := start; i < end; i++ {
		mark := "   "
		if i+1 == line {
			mark = ">>>"
		}
		fmt.Fprintf(&b, "%4d:%s %s\n", i+1, mark, lines[i])
		if i+1 == line {
			b.WriteString("     " + strings.Repeat(" ", col-1) + "^\n")
		}
	}
	return strings.TrimRight(b.String(), "\n")
}

func formatError(path, src string, err error) error {
	if ce, ok := err.(*ConvertError); ok {
		if ce.Line > 0 {
			return fmt.Errorf("%s:%d:%d: %s\n%s", path, ce.Line, ce.Column, ce.Msg, ce.Snip)
		}
		return fmt.Errorf("%s: %s\n%s", path, ce.Msg, ce.Snip)
	}
	if el, ok := err.(scanner.ErrorList); ok && len(el) > 0 {
		e := el[0]
		line := int(e.Pos.Line)
		col := int(e.Pos.Column)
		return fmt.Errorf("%s:%d:%d: %s\n%s", path, line, col, e.Msg, arrowSnippet(src, line, col))
	}
	return fmt.Errorf("%s: %v", path, err)
}
