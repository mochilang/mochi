package st

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

type AST struct {
	Statements []Stmt `json:"statements"`
}

type Stmt struct {
	Kind       string `json:"kind"`
	Name       string `json:"name,omitempty"`
	Expr       string `json:"expr,omitempty"`
	Cond       string `json:"cond,omitempty"`
	Start      string `json:"start,omitempty"`
	End        string `json:"end,omitempty"`
	Collection string `json:"collection,omitempty"`
	Body       []Stmt `json:"body,omitempty"`
	Else       []Stmt `json:"else,omitempty"`
	Line       int    `json:"line,omitempty"`
	Column     int    `json:"column,omitempty"`
}

func parseCLI(src string) (*AST, error) {
	root, err := repoRoot()
	if err != nil {
		return nil, err
	}
	tmp, err := os.CreateTemp("", "stsrc_*.st")
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
	cmd := exec.Command("go", "run", filepath.Join(root, "tools", "stast"), tmp.Name())
	var out bytes.Buffer
	var stderr bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		msg := strings.TrimSpace(stderr.String())
		if msg == "" {
			msg = err.Error()
		}
		return nil, fmt.Errorf("stast: %s", msg)
	}
	var ast AST
	if err := json.Unmarshal(out.Bytes(), &ast); err != nil {
		return nil, err
	}
	return &ast, nil
}

func convertAST(ast *AST, src string) ([]byte, error) {
	var out strings.Builder
	for _, s := range ast.Statements {
		if s.Kind == "unknown" {
			return nil, fmt.Errorf("%s", formatError(src, s.Line, s.Column, "unsupported statement"))
		}
		writeStmt(&out, s, 0)
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible statements found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

func writeStmt(out *strings.Builder, s Stmt, indent int) {
	ind := strings.Repeat("  ", indent)
	switch s.Kind {
	case "assign":
		out.WriteString(ind)
		out.WriteString("var ")
		out.WriteString(s.Name)
		if s.Expr != "" {
			out.WriteString(" = ")
			out.WriteString(s.Expr)
		}
		out.WriteByte('\n')
	case "print":
		out.WriteString(ind)
		out.WriteString("print(")
		out.WriteString(s.Expr)
		out.WriteString(")\n")
	case "return":
		out.WriteString(ind)
		out.WriteString("return ")
		out.WriteString(s.Expr)
		out.WriteByte('\n')
	case "while":
		out.WriteString(ind)
		out.WriteString("while ")
		out.WriteString(s.Cond)
		out.WriteString(" {\n")
		for _, b := range s.Body {
			writeStmt(out, b, indent+1)
		}
		out.WriteString(ind)
		out.WriteString("}\n")
	case "for":
		out.WriteString(ind)
		out.WriteString("for ")
		out.WriteString(s.Name)
		out.WriteString(" in ")
		out.WriteString(s.Start)
		out.WriteString("..")
		out.WriteString(s.End)
		out.WriteString(" {\n")
		for _, b := range s.Body {
			writeStmt(out, b, indent+1)
		}
		out.WriteString(ind)
		out.WriteString("}\n")
	case "foreach":
		out.WriteString(ind)
		out.WriteString("for ")
		out.WriteString(s.Name)
		out.WriteString(" in ")
		out.WriteString(s.Collection)
		out.WriteString(" {\n")
		for _, b := range s.Body {
			writeStmt(out, b, indent+1)
		}
		out.WriteString(ind)
		out.WriteString("}\n")
	case "if":
		out.WriteString(ind)
		out.WriteString("if (")
		out.WriteString(s.Cond)
		out.WriteString(") {\n")
		for _, b := range s.Body {
			writeStmt(out, b, indent+1)
		}
		out.WriteString(ind)
		out.WriteString("}")
		if len(s.Else) > 0 {
			out.WriteString(" else {\n")
			for _, b := range s.Else {
				writeStmt(out, b, indent+1)
			}
			out.WriteString(ind)
			out.WriteString("}\n")
		} else {
			out.WriteByte('\n')
		}
	}
}

func repoRoot() (string, error) {
	dir, err := os.Getwd()
	if err != nil {
		return "", err
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir, nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return "", os.ErrNotExist
}
