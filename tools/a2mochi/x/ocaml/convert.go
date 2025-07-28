package ocaml

import (
	_ "embed"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"mochi/ast"
	"mochi/parser"
)

//go:embed ocaml_ast.js
var ocamlParser string

// Program represents a parsed OCaml source file.
type Program struct {
	Funcs  []Func  `json:"funcs"`
	Prints []Print `json:"prints"`
	Types  []Type  `json:"types"`
	Vars   []Var   `json:"vars"`
}

type Func struct {
	Name   string   `json:"name"`
	Params []string `json:"params"`
	Body   string   `json:"body"`
}

type Var struct {
	Name    string `json:"name"`
	Expr    string `json:"expr"`
	Mutable bool   `json:"mutable"`
}

type Print struct {
	Expr string `json:"expr"`
}

type Type struct {
	Name   string  `json:"name"`
	Fields []Field `json:"fields"`
}

type Field struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

// Parse parses OCaml source code using the embedded tree-sitter script.
func Parse(src string) (*Program, error) {
	if _, err := exec.LookPath("node"); err != nil {
		return nil, fmt.Errorf("node not installed")
	}
	tmp, err := os.CreateTemp("", "ocaml-src-*.ml")
	if err != nil {
		return nil, err
	}
	if _, err := tmp.WriteString(src); err != nil {
		os.Remove(tmp.Name())
		return nil, err
	}
	tmp.Close()
	defer os.Remove(tmp.Name())

	scriptFile, err := os.CreateTemp("", "ocaml-parse-*.js")
	if err != nil {
		os.Remove(tmp.Name())
		return nil, err
	}
	if _, err := scriptFile.WriteString(ocamlParser); err != nil {
		os.Remove(tmp.Name())
		os.Remove(scriptFile.Name())
		return nil, err
	}
	scriptFile.Close()
	defer os.Remove(scriptFile.Name())

	root, _ := repoRoot()
	cmd := exec.Command("node", scriptFile.Name(), tmp.Name())
	if root != "" {
		cmd.Dir = root
		cmd.Env = append(os.Environ(), "NODE_PATH="+filepath.Join(root, "node_modules"))
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		return nil, fmt.Errorf("node error: %w\n%s", err, out)
	}
	var prog Program
	if err := json.Unmarshal(out, &prog); err != nil {
		return nil, err
	}
	return &prog, nil
}

// ConvertSource converts a Program into Mochi source code.
func ConvertSource(p *Program) (string, error) {
	if p == nil {
		return "", fmt.Errorf("nil program")
	}
	return string(formatProgram(p)), nil
}

// Convert converts a Program into a Mochi AST node.
func Convert(p *Program) (*ast.Node, error) {
	src, err := ConvertSource(p)
	if err != nil {
		return nil, err
	}
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	return ast.FromProgram(prog), nil
}

func formatProgram(p *Program) []byte {
	var out []byte
	b := &out
	appendLine := func(s string) {
		*b = append(*b, s...)
		*b = append(*b, '\n')
	}
	for _, typ := range p.Types {
		appendLine("type " + typ.Name + formatFields(typ.Fields))
	}
	for _, fn := range p.Funcs {
		line := "fun " + fn.Name + "(" + strings.Join(fn.Params, ", ") + ") {"
		appendLine(line)
		body := strings.ReplaceAll(fn.Body, "print_endline", "print")
		body = strings.ReplaceAll(body, "string_of_int", "str")
		body = strings.ReplaceAll(body, "string_of_float", "str")
		body = strings.ReplaceAll(body, "string_of_bool", "str")
		body = strings.ReplaceAll(body, "str (", "str(")
		body = strings.ReplaceAll(body, "print (", "print(")
		body = strings.ReplaceAll(body, "not ", "!")
		appendLine("  " + body)
		appendLine("}")
	}
	for _, v := range p.Vars {
		prefix := "let "
		if v.Mutable {
			prefix = "var "
		}
		appendLine(prefix + v.Name + maybeExpr(v.Expr))
	}
	for _, pr := range p.Prints {
		line := strings.TrimSpace(pr.Expr)
		line = strings.ReplaceAll(line, "string_of_int", "")
		line = strings.ReplaceAll(line, "string_of_float", "")
		line = strings.ReplaceAll(line, "string_of_bool", "")
		line = strings.ReplaceAll(line, "str (", "str(")
		line = strings.ReplaceAll(line, "print (", "print(")
		line = strings.ReplaceAll(line, "not ", "!")
		if strings.HasPrefix(line, "print_endline") {
			line = strings.TrimSpace(strings.TrimPrefix(line, "print_endline"))
			line = strings.TrimSuffix(strings.TrimPrefix(line, "("), ")")
			if strings.HasPrefix(line, "String.concat") {
				line = simplifyConcat(line)
			}
			if strings.HasPrefix(line, "str(") && strings.HasSuffix(line, ")") {
				line = strings.TrimSuffix(strings.TrimPrefix(line, "str("), ")")
			}
			appendLine("print(" + strings.TrimSpace(line) + ")")
		} else {
			if strings.HasPrefix(line, "str(") && strings.HasSuffix(line, ")") {
				line = strings.TrimSuffix(strings.TrimPrefix(line, "str("), ")")
			}
			appendLine("print(" + strings.TrimSpace(line) + ")")
		}
	}
	return out
}

func formatFields(fields []Field) string {
	if len(fields) == 0 {
		return " {}"
	}
	var b strings.Builder
	b.WriteString(" {\n")
	for _, f := range fields {
		b.WriteString("  ")
		b.WriteString(f.Name)
		if f.Type != "" {
			b.WriteString(": ")
			b.WriteString(mapOcamlType(f.Type))
		}
		b.WriteByte('\n')
	}
	b.WriteString("}")
	return b.String()
}

func maybeExpr(expr string) string {
	if expr == "" {
		return ""
	}
	return " = " + expr
}

func mapOcamlType(t string) string {
	t = strings.TrimSpace(t)
	switch t {
	case "", "unit":
		return ""
	case "int":
		return "int"
	case "float":
		return "float"
	case "string":
		return "string"
	case "bool":
		return "bool"
	}
	if strings.HasSuffix(t, " list") {
		inner := mapOcamlType(strings.TrimSpace(t[:len(t)-5]))
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasSuffix(t, " array") {
		inner := mapOcamlType(strings.TrimSpace(t[:len(t)-6]))
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasSuffix(t, " option") {
		return mapOcamlType(strings.TrimSpace(t[:len(t)-7]))
	}
	return t
}

func simplifyConcat(expr string) string {
	// handles String.concat " " ["text"]
	expr = strings.TrimSpace(strings.TrimPrefix(expr, "String.concat"))
	if strings.HasPrefix(expr, "\" \"") {
		idx := strings.Index(expr, "[")
		if idx != -1 {
			rest := expr[idx+1:]
			rest = strings.TrimSuffix(strings.TrimSpace(rest), "]")
			return rest
		}
	}
	return expr
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
