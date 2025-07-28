//go:build slow

package scala

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"

	"mochi/ast"
	"mochi/parser"
)

// Node represents a single top level declaration parsed from Scala.
type Node struct {
	Kind   string  `json:"kind"`
	Name   string  `json:"name,omitempty"`
	Params []Param `json:"params,omitempty"`
	Ret    string  `json:"ret,omitempty"`
	Body   string  `json:"body,omitempty"`
}

// Param describes a function parameter.
type Param struct {
	Name string `json:"name"`
}

// File represents the JSON AST produced by parser.scala.
type File struct {
	Stats []Node `json:"stats"`
}

var parserPath string

func init() {
	_, file, _, _ := runtime.Caller(0)
	parserPath = filepath.Join(filepath.Dir(file), "parser.scala")
}

// Parse runs the bundled Scala parser to obtain a JSON AST.
func Parse(src string) (*File, error) {
	tmp, err := os.CreateTemp("", "scalasrc_*.scala")
	if err != nil {
		return nil, err
	}
	if _, err := tmp.WriteString(src); err != nil {
		tmp.Close()
		os.Remove(tmp.Name())
		return nil, err
	}
	tmp.Close()
	defer os.Remove(tmp.Name())

	cmd := exec.Command("scala", parserPath, tmp.Name())
	var out strings.Builder
	cmd.Stdout = &out
	cmd.Stderr = &out
	if err := cmd.Run(); err != nil {
		return nil, fmt.Errorf("scala parser: %v\n%s", err, out.String())
	}
	var f File
	if err := json.Unmarshal([]byte(out.String()), &f); err != nil {
		return nil, err
	}
	return &f, nil
}

// ConvertSource converts the parsed AST into Mochi source code.
func ConvertSource(f *File, orig string) (string, error) {
	var b strings.Builder
	b.WriteString("// Converted from Scala\n")
	if orig != "" {
		b.WriteString("/*\n")
		b.WriteString(orig)
		if !strings.HasSuffix(orig, "\n") {
			b.WriteByte('\n')
		}
		b.WriteString("*/\n")
	}
	for _, st := range f.Stats {
		switch st.Kind {
		case "val":
			b.WriteString("let ")
			b.WriteString(st.Name)
			b.WriteByte('\n')
		case "def":
			b.WriteString("fun ")
			b.WriteString(st.Name)
			b.WriteByte('(')
			for i, p := range st.Params {
				if i > 0 {
					b.WriteString(", ")
				}
				b.WriteString(p.Name)
			}
			b.WriteByte(')')
			b.WriteString(" {\n")
			if st.Body != "" {
				for _, line := range strings.Split(st.Body, "\n") {
					if strings.TrimSpace(line) == "" {
						continue
					}
					b.WriteString("  ")
					b.WriteString(line)
					b.WriteByte('\n')
				}
			}
			b.WriteString("}\n")
		}
	}
	return b.String(), nil
}

// Convert converts the parsed AST to a Mochi AST node.
func Convert(f *File) (*ast.Node, error) {
	src, err := ConvertSource(f, "")
	if err != nil {
		return nil, err
	}
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	return ast.FromProgram(prog), nil
}
