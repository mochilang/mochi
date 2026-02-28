//go:build slow

package scheme

import (
	"fmt"
	"os"
	"strings"

	"mochi/ast"
	"mochi/transpiler/meta"
)

// Print renders the Mochi AST node to source code with a standard header.
func Print(node *ast.Node, src string) (string, error) {
	if node == nil {
		return "", fmt.Errorf("nil node")
	}
	var code strings.Builder
	if err := ast.Fprint(&code, node); err != nil {
		return "", err
	}
	if !strings.HasSuffix(code.String(), "\n") {
		code.WriteByte('\n')
	}
	var out strings.Builder
	out.Write(meta.Header("//"))
	out.WriteString("/*\n")
	out.WriteString(src)
	if !strings.HasSuffix(src, "\n") {
		out.WriteByte('\n')
	}
	out.WriteString("*/\n")
	out.WriteString(code.String())
	return out.String(), nil
}

// ConvertFileSource reads a Scheme file and converts it to Mochi source.
func ConvertFileSource(path string) (string, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return "", err
	}
	src := string(data)
	prog, err := Parse(src)
	if err != nil {
		return "", err
	}
	node, err := Transform(prog)
	if err != nil {
		return "", err
	}
	return Print(node, src)
}

// ConvertFile reads a Scheme file and returns a Mochi AST node.
func ConvertFile(path string) (*ast.Node, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	prog, err := Parse(string(data))
	if err != nil {
		return nil, err
	}
	return Transform(prog)
}
