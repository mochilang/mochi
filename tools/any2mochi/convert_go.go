package any2mochi

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/parser"
	"go/printer"
	"go/token"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
	go2 "mochi/tools/go2mochi"
)

// ConvertGo converts Go source code to Mochi. It first attempts to translate
// the source using the built-in Go parser. If that fails (e.g. due to
// unsupported constructs) it falls back to using the Go language server to
// generate simple stubs.
func ConvertGo(src string) ([]byte, error) {
	if out, err := go2.ConvertSource([]byte(src)); err == nil {
		if len(out) > 0 && out[len(out)-1] != '\n' {
			out = append(out, '\n')
		}
		return out, nil
	}

	ls := Servers["go"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	writeGoSymbols(&out, nil, syms, src, ls)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertGoFile reads the Go file at path and converts it to Mochi.
func ConvertGoFile(path string) ([]byte, error) {
	if out, err := go2.Convert(path); err == nil {
		if len(out) > 0 && out[len(out)-1] != '\n' {
			out = append(out, '\n')
		}
		return out, nil
	}
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertGo(string(data))
}

func writeGoSymbols(out *strings.Builder, prefix []string, syms []protocol.DocumentSymbol, src string, ls LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case protocol.SymbolKindFunction, protocol.SymbolKindMethod:
			params, ret := parseGoDetail(s.Detail)
			if len(params) == 0 && ret == "" {
				if hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, s.SelectionRange.Start); err == nil {
					if mc, ok := hov.Contents.(protocol.MarkupContent); ok {
						params, ret = parseGoDetail(&mc.Value)
					}
				}
			}
			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteByte('(')
			for i, p := range params {
				if i > 0 {
					out.WriteString(", ")
				}
				out.WriteString(p.name)
				if p.typ != "" {
					out.WriteString(": ")
					out.WriteString(p.typ)
				}
			}
			out.WriteByte(')')
			if ret != "" && ret != "void" {
				out.WriteString(": ")
				out.WriteString(ret)
			}
			out.WriteString(" {}\n")
		case protocol.SymbolKindStruct, protocol.SymbolKindClass, protocol.SymbolKindInterface:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			fields := []protocol.DocumentSymbol{}
			rest := []protocol.DocumentSymbol{}
			for _, c := range s.Children {
				if c.Kind == protocol.SymbolKindField {
					fields = append(fields, c)
				} else {
					rest = append(rest, c)
				}
			}
			if len(fields) == 0 && len(rest) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, f := range fields {
					out.WriteString("  ")
					out.WriteString(f.Name)
					if f.Detail != nil && strings.TrimSpace(*f.Detail) != "" {
						out.WriteString(": ")
						out.WriteString(strings.TrimSpace(*f.Detail))
					}
					out.WriteByte('\n')
				}
				out.WriteString("}\n")
				if len(rest) > 0 {
					writeGoSymbols(out, nameParts, rest, src, ls)
				}
			}
		case protocol.SymbolKindVariable, protocol.SymbolKindConstant:
			if len(prefix) == 0 {
				out.WriteString("let ")
				out.WriteString(strings.Join(nameParts, "."))
				if s.Detail != nil && strings.TrimSpace(*s.Detail) != "" {
					out.WriteString(": ")
					out.WriteString(strings.TrimSpace(*s.Detail))
				}
				out.WriteByte('\n')
			}
			if len(s.Children) > 0 {
				writeGoSymbols(out, nameParts, s.Children, src, ls)
			}
		}
		if len(s.Children) > 0 && s.Kind != protocol.SymbolKindStruct && s.Kind != protocol.SymbolKindClass && s.Kind != protocol.SymbolKindInterface && s.Kind != protocol.SymbolKindVariable && s.Kind != protocol.SymbolKindConstant {
			writeGoSymbols(out, nameParts, s.Children, src, ls)
		}
	}
}

type goParam struct{ name, typ string }

func parseGoDetail(detail *string) ([]goParam, string) {
	if detail == nil {
		return nil, ""
	}
	d := strings.TrimSpace(*detail)
	if strings.HasPrefix(d, "```") {
		d = strings.Trim(d, "`")
		d = strings.TrimSpace(d)
		if strings.HasPrefix(d, "go\n") {
			d = strings.TrimPrefix(d, "go\n")
			d = strings.TrimSpace(d)
		}
	}
	if d == "" {
		return nil, ""
	}
	if !strings.HasPrefix(d, "func") {
		return nil, strings.TrimSpace(d)
	}
	src := "package p\n" + d + "{}"
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, "src.go", src, 0)
	if err != nil || len(file.Decls) == 0 {
		return nil, ""
	}
	fn, ok := file.Decls[0].(*ast.FuncDecl)
	if !ok {
		return nil, ""
	}
	var params []goParam
	if fn.Type.Params != nil {
		for _, p := range fn.Type.Params.List {
			typ := exprString(fset, p.Type)
			if len(p.Names) == 0 {
				params = append(params, goParam{"", typ})
				continue
			}
			for _, n := range p.Names {
				params = append(params, goParam{n.Name, typ})
			}
		}
	}
	ret := ""
	if fn.Type.Results != nil && len(fn.Type.Results.List) == 1 && len(fn.Type.Results.List[0].Names) == 0 {
		ret = exprString(fset, fn.Type.Results.List[0].Type)
	}
	return params, ret
}

func exprString(fset *token.FileSet, e ast.Expr) string {
	var b bytes.Buffer
	printer.Fprint(&b, fset, e)
	return b.String()
}
