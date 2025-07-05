package any2mochi

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"strings"
)

// ConvertGo converts Go source code to a minimal Mochi representation.
// It parses the source using the Go standard library and emits one Mochi
// function stub per top-level function including its parameter names.
func ConvertGo(src string) ([]byte, error) {
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, "input.go", src, 0)
	if err != nil {
		return nil, err
	}
	var out strings.Builder
	for _, decl := range file.Decls {
		fn, ok := decl.(*ast.FuncDecl)
		if !ok {
			continue
		}
		out.WriteString("fun ")
		out.WriteString(fn.Name.Name)
		out.WriteString("(")
		var params []string
		if fn.Type.Params != nil {
			for _, field := range fn.Type.Params.List {
				for _, name := range field.Names {
					params = append(params, name.Name)
				}
			}
		}
		out.WriteString(strings.Join(params, ", "))
		out.WriteString(") {}\n")
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible functions found")
	}
	return []byte(out.String()), nil
}

// ConvertGoFile reads the Go file at path and converts it to Mochi.
func ConvertGoFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertGo(string(data))
}
