//go:build slow

// Package ocaml provides a limited OCaml to Mochi converter using tree-sitter for parsing.
package ocaml

import (
	_ "embed"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
	"time"

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

	// Source holds the original OCaml code used to build the Program.
	Source string `json:"-"`
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
	prog.Source = src
	return &prog, nil
}

// ConvertSource converts a Program into Mochi source code.
func ConvertSource(p *Program) (string, error) {
	if p == nil {
		return "", fmt.Errorf("nil program")
	}
	var b strings.Builder
	b.WriteString(header(p.Source))
	b.Write(formatProgram(p))
	return b.String(), nil
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
		body = replaceBuiltins(body)
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
	handlePrint := func(line string) {
		line = strings.TrimSpace(line)
		line = replaceBuiltins(line)
		line = strings.ReplaceAll(line, "str (", "str(")
		line = strings.ReplaceAll(line, "print (", "print(")
		line = strings.ReplaceAll(line, "not ", "!")
		noNL := strings.ReplaceAll(line, "\n", " ")
		varRe := regexp.MustCompile(`^let ([a-zA-Z0-9_']+) = ref 0 in\s*print_endline`)
		letRe := regexp.MustCompile(`^let ([a-zA-Z0-9_']+) = 0 in\s*print_endline`)
		if m := varRe.FindStringSubmatch(noNL); m != nil {
			appendLine("var " + m[1] + ": int")
			appendLine("print(" + m[1] + ")")
			return
		}
		if m := letRe.FindStringSubmatch(noNL); m != nil {
			appendLine("let " + m[1] + ": int")
			appendLine("print(" + m[1] + ")")
			return
		}
		if strings.HasPrefix(line, "print_endline") {
			line = strings.TrimSpace(strings.TrimPrefix(line, "print_endline"))
			line = strings.TrimSuffix(strings.TrimPrefix(line, "("), ")")
			if strings.HasPrefix(line, "String.concat") {
				line = simplifyConcat(line)
			}
			if strings.HasPrefix(line, "str(") && strings.HasSuffix(line, ")") {
				line = strings.TrimSuffix(strings.TrimPrefix(line, "str("), ")")
			} else if strings.HasPrefix(line, "str ") {
				line = strings.TrimSpace(strings.TrimPrefix(line, "str"))
			}
			appendLine("print(" + strings.TrimSpace(line) + ")")
		} else {
			if strings.HasPrefix(line, "str(") && strings.HasSuffix(line, ")") {
				line = strings.TrimSuffix(strings.TrimPrefix(line, "str("), ")")
			} else if strings.HasPrefix(line, "str ") {
				line = strings.TrimSpace(strings.TrimPrefix(line, "str"))
			}
			appendLine("print(" + strings.TrimSpace(line) + ")")
		}
	}

	for _, pr := range p.Prints {
		line := strings.TrimSpace(pr.Expr)
		if strings.HasPrefix(line, "let ") {
			line = strings.ReplaceAll(line, " in\n", ";")
			line = strings.ReplaceAll(line, " in ", ";")
			parts := strings.Split(line, ";")
			for _, part := range parts {
				stmt := strings.TrimSpace(part)
				if stmt == "" {
					continue
				}
				if strings.HasPrefix(stmt, "let ") {
					appendLine(stmt)
				} else {
					handlePrint(stmt)
				}
			}
			continue
		}
		if strings.Contains(line, ";\n") {
			parts := regexp.MustCompile(`;\s*\n`).Split(line, -1)
			for _, part := range parts {
				stmt := strings.TrimSpace(part)
				if stmt == "" {
					continue
				}
				handlePrint(stmt)
			}
		} else {
			handlePrint(line)
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
	// handles String.concat " " [expr]
	expr = strings.TrimSpace(strings.TrimPrefix(expr, "String.concat"))
	if !strings.HasPrefix(expr, "\" \"") {
		return expr
	}
	i := strings.Index(expr, "[")
	if i == -1 {
		return expr
	}
	s := expr[i+1:]
	depth := 1
	end := -1
	for idx, r := range s {
		if r == '[' {
			depth++
		} else if r == ']' {
			depth--
			if depth == 0 {
				end = idx
				break
			}
		}
	}
	if end == -1 {
		return expr
	}
	inner := strings.TrimSpace(s[:end])
	return inner
}

func replaceBuiltins(s string) string {
	s = strings.ReplaceAll(s, "string_of_int", "str")
	s = strings.ReplaceAll(s, "string_of_float", "str")
	s = strings.ReplaceAll(s, "string_of_bool", "str")
	s = strings.ReplaceAll(s, "String.length", "len")
	strLen := regexp.MustCompile(`len\s+"([^"]+)"`)
	s = strLen.ReplaceAllString(s, `len("$1")`)
	listLen := regexp.MustCompile(`List\.length\s*\[([^\]]*)\]`)
	s = listLen.ReplaceAllString(s, `len([$1])`)
	s = strings.ReplaceAll(s, "List.length", "len")
	listNth := regexp.MustCompile(`List\.nth\s*\(([^\)]+)\)\s*(\d+)`)
	s = listNth.ReplaceAllString(s, `$1[$2]`)
	s = strings.ReplaceAll(s, ";", ",")
	return s
}

func header(src string) string {
	loc := time.FixedZone("GMT+7", 7*3600)
	t := gitTime().In(loc)
	var b strings.Builder
	fmt.Fprintf(&b, "// Generated by a2mochi OCaml converter v%s on %s\n", version(), t.Format("2006-01-02 15:04 -0700"))
	if src != "" {
		b.WriteString("/*\n")
		b.WriteString(src)
		if !strings.HasSuffix(src, "\n") {
			b.WriteByte('\n')
		}
		b.WriteString("*/\n")
	}
	return b.String()
}

func version() string {
	root, _ := repoRoot()
	if root == "" {
		return "dev"
	}
	data, err := os.ReadFile(filepath.Join(root, "VERSION"))
	if err != nil {
		return "dev"
	}
	return strings.TrimSpace(string(data))
}

func gitTime() time.Time {
	root, _ := repoRoot()
	if root == "" {
		return time.Now()
	}
	cmd := exec.Command("git", "-C", root, "log", "-1", "--format=%cI")
	out, err := cmd.Output()
	if err != nil {
		return time.Now()
	}
	t, err := time.Parse(time.RFC3339, strings.TrimSpace(string(out)))
	if err != nil {
		return time.Now()
	}
	return t
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
