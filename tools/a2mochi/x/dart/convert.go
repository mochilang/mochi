//go:build slow

package dart

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"

	"mochi/ast"
	"mochi/parser"
	"mochi/transpiler/meta"
)

// Param represents a Dart function parameter.
type Param struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

// Function represents a parsed Dart function.
type Function struct {
	Name   string   `json:"name"`
	Params []Param  `json:"params"`
	Ret    string   `json:"ret"`
	Body   []string `json:"body"`
	Start  int      `json:"start"`
	End    int      `json:"end"`
	Doc    string   `json:"doc,omitempty"`
}

// Field represents a Dart class field.
type Field struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

// Class represents a Dart class definition.
type Class struct {
	Name   string  `json:"name"`
	Fields []Field `json:"fields"`
	Start  int     `json:"start"`
	End    int     `json:"end"`
	Doc    string  `json:"doc,omitempty"`
}

// Node is the root of a parsed Dart file.
type Node struct {
	Functions []Function `json:"functions"`
	Classes   []Class    `json:"classes"`
	Src       string     `json:"-"`
}

// Parse parses Dart source into a Node using the dartast helper.
func Parse(src string) (*Node, error) {
	funcs, classes, err := parseInternal(src)
	if err != nil {
		return nil, err
	}
	return &Node{Functions: funcs, Classes: classes, Src: src}, nil
}

func parseInternal(src string) ([]Function, []Class, error) {
	if path, err := exec.LookPath("dartast"); err == nil {
		cmd := exec.Command(path)
		cmd.Stdin = strings.NewReader(src)
		var out bytes.Buffer
		var errBuf bytes.Buffer
		cmd.Stdout = &out
		cmd.Stderr = &errBuf
		if err := cmd.Run(); err != nil {
			if errBuf.Len() > 0 {
				return nil, nil, fmt.Errorf("%v: %s", err, errBuf.String())
			}
			return nil, nil, err
		}
		return decode(out.Bytes())
	}
	root, err := repoRoot()
	if err != nil {
		return nil, nil, err
	}
	cmd := exec.Command("go", "run", filepath.Join(root, "cmd", "dartast"))
	cmd.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	var errBuf bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &errBuf
	if err := cmd.Run(); err != nil {
		if errBuf.Len() > 0 {
			return nil, nil, fmt.Errorf("%v: %s", err, errBuf.String())
		}
		return nil, nil, err
	}
	return decode(out.Bytes())
}

func decode(data []byte) ([]Function, []Class, error) {
	var a struct {
		Functions []Function `json:"functions"`
		Classes   []Class    `json:"classes"`
	}
	if err := json.Unmarshal(data, &a); err != nil {
		return nil, nil, err
	}
	return a.Functions, a.Classes, nil
}

// ConvertSource converts the parsed Dart node into Mochi source code.
func ConvertSource(n *Node) (string, error) {
	var b strings.Builder
	b.Write(meta.Header("// "))
	b.WriteString("/*\n")
	b.WriteString(n.Src)
	if !strings.HasSuffix(n.Src, "\n") {
		b.WriteByte('\n')
	}
	b.WriteString("*/\n")
	for _, v := range parseTopLevelVars(n.Src, n.Functions, n.Classes) {
		b.WriteString(v)
		b.WriteByte('\n')
	}
	for _, c := range n.Classes {
		b.WriteString("type ")
		b.WriteString(c.Name)
		b.WriteString(" {\n")
		for _, f := range c.Fields {
			b.WriteString("  ")
			b.WriteString(f.Name)
			if t := toMochiType(f.Type); t != "" && t != "any" {
				b.WriteString(": ")
				b.WriteString(t)
			}
			b.WriteByte('\n')
		}
		b.WriteString("}\n")
	}
	for _, f := range n.Functions {
		b.WriteString("fun ")
		b.WriteString(f.Name)
		b.WriteByte('(')
		for i, p := range f.Params {
			if i > 0 {
				b.WriteString(", ")
			}
			b.WriteString(p.Name)
			if t := toMochiType(p.Type); t != "" && t != "any" {
				b.WriteString(": ")
				b.WriteString(t)
			}
		}
		b.WriteByte(')')
		if r := toMochiType(f.Ret); r != "" {
			b.WriteString(": ")
			b.WriteString(r)
		}
		b.WriteString(" {\n")
		for _, line := range f.Body {
			b.WriteString(convertBodyLine(line))
			b.WriteByte('\n')
		}
		b.WriteString("}\n")
	}
	return b.String(), nil
}

// Convert converts a parsed Dart node into a Mochi AST.
func Convert(n *Node) (*ast.Node, error) {
	src, err := ConvertSource(n)
	if err != nil {
		return nil, err
	}
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	return ast.FromProgram(prog), nil
}

// --- Helpers copied from dartast and archived converter ---

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

var typedVarRe = regexp.MustCompile(`^(?:final|const)?\s*([A-Za-z_][A-Za-z0-9_<>,\[\]\? ]*)\s+([A-Za-z_][A-Za-z0-9_]*)\s*(=.*)?$`)

func parseTopLevelVars(src string, funcs []Function, classes []Class) []string {
	lines := strings.Split(src, "\n")
	skip := make([]bool, len(lines)+1)
	mark := func(start, end int) {
		for i := start; i <= end && i <= len(lines); i++ {
			if i >= 1 {
				skip[i] = true
			}
		}
	}
	for _, f := range funcs {
		mark(f.Start, f.End)
	}
	for _, c := range classes {
		mark(c.Start, c.End)
	}
	var vars []string
	for i, line := range lines {
		ln := i + 1
		if skip[ln] {
			continue
		}
		l := strings.TrimSpace(strings.TrimSuffix(line, ";"))
		if strings.HasPrefix(l, "var ") {
			vars = append(vars, convertQuotes("let "+strings.TrimSpace(l[4:])))
			continue
		}
		if m := typedVarRe.FindStringSubmatch(l); m != nil {
			typ := toMochiType(strings.TrimSpace(m[1]))
			name := m[2]
			val := strings.TrimSpace(strings.TrimPrefix(m[3], "="))
			stmt := "let " + name
			if typ != "" && typ != "any" {
				stmt += ": " + typ
			}
			if val != "" {
				stmt += " = " + val
			}
			vars = append(vars, convertQuotes(stmt))
		}
	}
	return vars
}

var quoteRe = regexp.MustCompile(`'([^']*)'`)

func convertQuotes(s string) string {
	return quoteRe.ReplaceAllStringFunc(s, func(q string) string {
		return "\"" + strings.Trim(q, "'") + "\""
	})
}

var forVarRe = regexp.MustCompile(`^(\s*)for \((?:var|final|const) ([A-Za-z_][A-Za-z0-9_]*) in ([^)]*)\)`)

func convertBodyLine(s string) string {
	s = strings.TrimSuffix(s, ";")
	s = forVarRe.ReplaceAllString(s, "${1}for ${2} in ${3}")
	if strings.HasPrefix(strings.TrimSpace(s), "let ") {
		s = strings.Replace(s, "let ", "var ", 1)
	}
	s = fixUnaryNeg(s)
	s = convertArrowFunc(s)
	s = convertTernaryPrint(s)
	s = convertTernary(s)
	s = convertSpread(s)
	s = convertLength(s)
	return convertQuotes(s)
}

var arrowRe = regexp.MustCompile(`\(([^()]*)\)\s*=>`)

func convertArrowFunc(s string) string {
	return arrowRe.ReplaceAllString(s, "fun($1) =>")
}

var unaryNegRe = regexp.MustCompile(`([+\-*/])\s*-([A-Za-z0-9_]+)`) // e.g., "+ -2" -> "+ (-2)"

func fixUnaryNeg(s string) string {
	return unaryNegRe.ReplaceAllString(s, "$1 (-$2)")
}

func convertTernaryPrint(s string) string {
	if m := printTernaryRe.FindStringSubmatch(s); m != nil {
		return fmt.Sprintf("print(if %s { %s } else { %s })", strings.TrimSpace(m[1]), strings.TrimSpace(m[2]), strings.TrimSpace(m[3]))
	}
	return s
}

var printTernaryRe = regexp.MustCompile(`^\s*print\(([^?]+)\?\s*([^:]+)\s*:\s*([^)]*)\)$`)

var ternaryRe = regexp.MustCompile(`([^?]+)\?\s*([^:]+)\s*:\s*(.+)`)

func convertTernary(s string) string {
	if ternaryRe.MatchString(s) {
		return ternaryRe.ReplaceAllString(s, "if $1 { $2 } else { $3 }")
	}
	return s
}

var spreadRe = regexp.MustCompile(`\[\.\.\.([A-Za-z_][A-Za-z0-9_]*)\s*,\s*([^\]]+)\]`)

func convertSpread(s string) string {
	if m := spreadPrintRe.FindStringSubmatch(s); m != nil {
		return fmt.Sprintf("print(append(%s, %s))", m[1], m[2])
	}
	return spreadRe.ReplaceAllString(s, "append($1, $2)")
}

var spreadPrintRe = regexp.MustCompile(`^print\("\[" \+ append\(([^,]+),\s*([^\)]+)\)\.join\("[, ]*"\) \+ "\]"\)$`)

var lengthRe = regexp.MustCompile(`([A-Za-z0-9_\]\)]+)\.length`)

func convertLength(s string) string {
	return lengthRe.ReplaceAllString(s, "len($1)")
}

func splitArgs(s string) []string {
	var parts []string
	depth := 0
	start := 0
	for i, r := range s {
		switch r {
		case '<':
			depth++
		case '>':
			if depth > 0 {
				depth--
			}
		case ',':
			if depth == 0 {
				parts = append(parts, strings.TrimSpace(s[start:i]))
				start = i + 1
			}
		}
	}
	if start < len(s) {
		parts = append(parts, strings.TrimSpace(s[start:]))
	}
	return parts
}

func toMochiType(t string) string {
	t = strings.TrimSpace(t)
	if strings.HasSuffix(t, "?") {
		t = strings.TrimSuffix(t, "?")
	}
	switch t {
	case "", "dynamic", "Object":
		return "any"
	case "int":
		return "int"
	case "double", "num":
		return "float"
	case "bool":
		return "bool"
	case "String":
		return "string"
	case "void":
		return ""
	}
	if strings.HasSuffix(t, ">") {
		if open := strings.Index(t, "<"); open != -1 {
			outer := strings.TrimSpace(t[:open])
			inner := strings.TrimSuffix(t[open+1:], ">")
			args := splitArgs(inner)
			switch outer {
			case "List", "Iterable", "Set":
				a := "any"
				if len(args) > 0 {
					if at := toMochiType(args[0]); at != "" {
						a = at
					}
				}
				return "list<" + a + ">"
			case "Map":
				if len(args) == 2 {
					k := toMochiType(args[0])
					if k == "" {
						k = "any"
					}
					v := toMochiType(args[1])
					if v == "" {
						v = "any"
					}
					return "map<" + k + ", " + v + ">"
				}
			case "Future":
				if len(args) == 1 {
					return toMochiType(args[0])
				}
			}
		}
	}
	return t
}
