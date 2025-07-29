package gox

import (
	"fmt"
	"go/ast"
	goparser "go/parser"
	"go/token"
	"strings"
)

// Program wraps a parsed Go file and its original lines.
type Program struct {
	File  *ast.File
	Fset  *token.FileSet
	Lines []string
}

// Parse parses Go source code into a Program.
func Parse(src string) (*Program, error) {
	if strings.TrimSpace(src) == "" {
		return nil, fmt.Errorf("empty source")
	}
	fset := token.NewFileSet()
	file, err := goparser.ParseFile(fset, "", src, goparser.ParseComments)
	if err != nil {
		return nil, err
	}
	return &Program{File: file, Fset: fset, Lines: strings.Split(src, "\n")}, nil
}
