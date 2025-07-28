package gox

import (
	"fmt"
	"go/ast"
	goparser "go/parser"
	"go/token"
	"os"
	"strings"
	"time"

	mochias "mochi/ast"
	mochiparser "mochi/parser"
)

// Node wraps a parsed Go file.
type Node struct {
	File  *ast.File
	Fset  *token.FileSet
	Lines []string
}

// Parse parses Go source code into a Node.
func Parse(src string) (*Node, error) {
	fset := token.NewFileSet()
	file, err := goparser.ParseFile(fset, "", src, goparser.ParseComments)
	if err != nil {
		return nil, err
	}
	return &Node{File: file, Fset: fset, Lines: strings.Split(src, "\n")}, nil
}

// Convert converts a parsed Go Node into a Mochi AST node.
// Only a tiny subset of Go is supported; for now this is unimplemented and
// returns an empty program.
func Convert(n *Node) (*mochias.Node, error) {
	if n == nil {
		return nil, fmt.Errorf("nil node")
	}
	prog, err := mochiparser.ParseString("")
	if err != nil {
		return nil, err
	}
	return mochias.FromProgram(prog), nil
}

// ConvertSource emits Mochi source code for the parsed Go node. Only a header
// comment and the original source code block are emitted for now.
func ConvertSource(n *Node) (string, error) {
	if n == nil {
		return "", fmt.Errorf("nil node")
	}
	version := strings.TrimSpace(readVersion())
	t := time.Now().In(time.FixedZone("GMT+7", 7*3600)).Format("2006-01-02 15:04:05")
	var b strings.Builder
	fmt.Fprintf(&b, "// a2mochi go v%s %s GMT+7\n", version, t)
	b.WriteString("/*\n")
	b.WriteString(strings.Join(n.Lines, "\n"))
	if len(n.Lines) == 0 || n.Lines[len(n.Lines)-1] != "" {
		b.WriteByte('\n')
	}
	b.WriteString("*/\n")
	return b.String(), nil
}

// readVersion reads the VERSION file at repo root.
func readVersion() string {
	data, err := os.ReadFile("VERSION")
	if err != nil {
		return "dev"
	}
	return string(data)
}
