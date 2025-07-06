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
)

// ConvertError provides a detailed error suitable for golden tests.
type ConvertError struct {
	Line int
	Msg  string
	Snip string
}

func (e *ConvertError) Error() string {
	if e.Line > 0 {
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
	return Convert(string(data))
}

func writeFunc(out *strings.Builder, fset *token.FileSet, fn *ast.FuncDecl) {
	if fn.Name == nil {
		return
	}
	name := fn.Name.Name
	if fn.Recv != nil && len(fn.Recv.List) > 0 {
		recv := exprString(fset, fn.Recv.List[0].Type)
		name = recv + "." + name
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
				out.WriteString(n.Name)
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
				out.WriteString(ts.Name.Name)
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
						out.WriteString(n.Name)
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
				out.WriteString(n.Name)
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
	return t
}

func formatParseError(src string, err error) error {
	if el, ok := err.(scanner.ErrorList); ok && len(el) > 0 {
		e := el[0]
		pos := e.Pos
		lines := strings.Split(src, "\n")
		line := int(pos.Line)
		start := line - 2
		if start < 0 {
			start = 0
		}
		end := line + 1
		if end > len(lines) {
			end = len(lines)
		}
		var snippet strings.Builder
		for i := start; i < end; i++ {
			marker := "   "
			if i == line-1 {
				marker = ">>>"
			}
			snippet.WriteString(fmt.Sprintf("%3d:%s %s\n", i+1, marker, lines[i]))
		}
		return &ConvertError{Line: line, Msg: e.Msg, Snip: strings.TrimRight(snippet.String(), "\n")}
	}
	return err
}
