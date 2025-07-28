package ts

import (
	_ "embed"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"strings"

	"mochi/ast"
	"mochi/parser"
)

//go:embed parse.ts
var tsParser string

// Node represents a top-level declaration parsed from TypeScript.
type Node struct {
	Kind     string   `json:"kind"`
	Name     string   `json:"name"`
	Node     string   `json:"node,omitempty"`
	Params   []Param  `json:"params,omitempty"`
	Ret      string   `json:"ret,omitempty"`
	Body     string   `json:"body,omitempty"`
	Fields   []Field  `json:"fields,omitempty"`
	Alias    string   `json:"alias,omitempty"`
	Variants []string `json:"variants,omitempty"`
}

type Param struct {
	Name string `json:"name"`
	Typ  string `json:"typ"`
}

type Field struct {
	Name string `json:"name"`
	Typ  string `json:"typ"`
}

// Parse parses TypeScript source code into a slice of Nodes using Deno.
func Parse(src string) ([]Node, error) {
	if _, err := exec.LookPath("deno"); err != nil {
		return nil, fmt.Errorf("deno not installed")
	}
	tmp, err := os.CreateTemp("", "ts-src-*.ts")
	if err != nil {
		return nil, err
	}
	if _, err := tmp.WriteString(src); err != nil {
		os.Remove(tmp.Name())
		return nil, err
	}
	tmp.Close()
	defer os.Remove(tmp.Name())
	scriptFile, err := os.CreateTemp("", "ts-parse-*.ts")
	if err != nil {
		os.Remove(tmp.Name())
		return nil, err
	}
	if _, err := scriptFile.WriteString(tsParser); err != nil {
		os.Remove(tmp.Name())
		os.Remove(scriptFile.Name())
		return nil, err
	}
	scriptFile.Close()
	defer os.Remove(scriptFile.Name())

	cmd := exec.Command("deno", "run", "--quiet", "--allow-read", "--allow-env", "--node-modules-dir=auto", scriptFile.Name(), tmp.Name())
	cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
	out, err := cmd.CombinedOutput()
	if err != nil {
		return nil, fmt.Errorf("deno error: %w\n%s", err, out)
	}
	var decls []Node
	if err := json.Unmarshal(out, &decls); err != nil {
		return nil, err
	}
	return decls, nil
}

// Convert converts parsed TypeScript nodes into a Mochi AST.
func ConvertSource(nodes []Node) (string, error) {
	var b strings.Builder
	for _, d := range nodes {
		switch d.Kind {
		case "var":
			writeVar(&b, d)
		case "funcvar":
			writeFuncVar(&b, d)
		case "func":
			writeFunc(&b, d)
		case "enum":
			writeEnum(&b, d)
		case "type":
			writeType(&b, d)
		case "alias":
			writeAlias(&b, d)
		}
	}
	return b.String(), nil
}

func Convert(nodes []Node) (*ast.Node, error) {
	src, err := ConvertSource(nodes)
	if err != nil {
		return nil, err
	}
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	return ast.FromProgram(prog), nil
}

func writeVar(b *strings.Builder, d Node) {
	b.WriteString("let ")
	b.WriteString(d.Name)
	if d.Ret != "" {
		b.WriteString(": ")
		b.WriteString(d.Ret)
	}
	b.WriteByte('\n')
}

func writeFuncVar(b *strings.Builder, d Node) {
	b.WriteString("let ")
	b.WriteString(d.Name)
	b.WriteString(" = ")
	writeFuncSignature(b, d)
}

func writeFunc(b *strings.Builder, d Node) {
	b.WriteString("fun ")
	b.WriteString(d.Name)
	writeFuncSignature(b, d)
}

func writeFuncSignature(b *strings.Builder, d Node) {
	b.WriteByte('(')
	for i, p := range d.Params {
		if i > 0 {
			b.WriteString(", ")
		}
		b.WriteString(p.Name)
		if p.Typ != "" {
			b.WriteString(": ")
			b.WriteString(p.Typ)
		}
	}
	b.WriteByte(')')
	if d.Ret != "" && d.Ret != "void" {
		b.WriteString(": ")
		b.WriteString(d.Ret)
	}
	b.WriteString(" {\n}")
	b.WriteByte('\n')
}

func writeEnum(b *strings.Builder, d Node) {
	b.WriteString("type ")
	b.WriteString(d.Name)
	b.WriteString(" {\n")
	for _, v := range d.Variants {
		b.WriteString("  ")
		b.WriteString(v)
		b.WriteByte('\n')
	}
	b.WriteString("}\n")
}

func writeType(b *strings.Builder, d Node) {
	b.WriteString("type ")
	b.WriteString(d.Name)
	b.WriteString(" {\n")
	for _, f := range d.Fields {
		b.WriteString("  ")
		b.WriteString(f.Name)
		if f.Typ != "" {
			b.WriteString(": ")
			b.WriteString(f.Typ)
		}
		b.WriteByte('\n')
	}
	b.WriteString("}\n")
}

func writeAlias(b *strings.Builder, d Node) {
	b.WriteString("type ")
	b.WriteString(d.Name)
	b.WriteString(" = ")
	b.WriteString(d.Alias)
	b.WriteByte('\n')
}

// --- Helpers copied from archived any2mochi ---

func findMatch(s string, openIdx int, open, close rune) int {
	depth := 0
	for i, r := range s[openIdx:] {
		if r == open {
			depth++
		} else if r == close {
			depth--
			if depth == 0 {
				return openIdx + i
			}
		}
	}
	return len(s)
}

func parseFilterMap(expr, lhs string) []string {
	filterIdx := strings.Index(expr, ".filter(")
	mapIdx := strings.Index(expr, ".map(")
	if filterIdx == -1 || mapIdx == -1 || mapIdx < filterIdx {
		return nil
	}
	list := strings.TrimSpace(expr[:filterIdx])
	fStart := filterIdx + len(".filter(")
	fEnd := findMatch(expr, fStart-1, '(', ')')
	if fEnd <= fStart {
		return nil
	}
	fPart := expr[fStart:fEnd]
	arrow := strings.Index(fPart, "=>")
	if arrow == -1 {
		return nil
	}
	iter := strings.TrimSpace(strings.Trim(fPart[:arrow], "() "))
	cond := strings.TrimSpace(strings.Trim(fPart[arrow+2:], "() "))
	mStart := mapIdx + len(".map(")
	mEnd := findMatch(expr, mStart-1, '(', ')')
	if mEnd <= mStart {
		return nil
	}
	mPart := expr[mStart:mEnd]
	arrow = strings.Index(mPart, "=>")
	if arrow == -1 {
		return nil
	}
	iter2 := strings.TrimSpace(strings.Trim(mPart[:arrow], "() "))
	body := strings.TrimSpace(strings.Trim(mPart[arrow+2:], "() "))
	if iter2 != "" {
		iter = iter2
	}
	var out []string
	out = append(out, lhs+" = from "+iter+" in "+list)
	out = append(out, "             where "+cond)
	out = append(out, "             select "+body)
	return out
}
