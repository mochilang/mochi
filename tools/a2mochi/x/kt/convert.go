//go:build slow

package kt

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"strings"

	"mochi/ast"
	"mochi/parser"
	"mochi/transpiler/meta"
)

// Node represents a top-level declaration parsed from Kotlin.
type Node struct {
	Kind   string  `json:"kind"`
	Name   string  `json:"name"`
	Params []Param `json:"params,omitempty"`
	Ret    string  `json:"ret,omitempty"`
	Fields []Field `json:"fields,omitempty"`
}

type Param struct {
	Name string `json:"name"`
	Typ  string `json:"typ"`
}

type Field struct {
	Name string `json:"name"`
	Typ  string `json:"typ"`
}

// Parse parses Kotlin source using kotlinc's -Xdump-declarations-to flag.
func Parse(src string) ([]Node, error) {
	ast, err := parseKotlinc(src)
	if err != nil {
		return nil, err
	}
	return toNodes(ast), nil
}

type astJSON struct {
	Classes   []cls   `json:"classes"`
	Functions []fn    `json:"functions"`
	Vars      []vdecl `json:"vars"`
}

type cls struct {
	Name    string  `json:"name"`
	Kind    string  `json:"kind,omitempty"`
	Data    bool    `json:"data,omitempty"`
	Sealed  bool    `json:"sealed,omitempty"`
	Extends string  `json:"extends,omitempty"`
	Fields  []field `json:"fields"`
	Methods []fn    `json:"methods"`
}

type field struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

type vdecl struct {
	Name  string `json:"name"`
	Type  string `json:"type,omitempty"`
	Value string `json:"value"`
}

type fn struct {
	Name   string      `json:"name"`
	Params []paramDecl `json:"params"`
	Ret    string      `json:"ret,omitempty"`
}

type paramDecl struct {
	Name string `json:"name"`
	Type string `json:"type,omitempty"`
}

func parseKotlinc(src string) (*astJSON, error) {
	if _, err := exec.LookPath("kotlinc"); err != nil {
		return nil, fmt.Errorf("kotlinc not installed")
	}
	tmp, err := os.CreateTemp("", "ktsrc_*.kt")
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

	outFile := tmp.Name() + ".json"
	cmd := exec.Command("kotlinc", tmp.Name(), "-Xdump-declarations-to="+outFile)
	if err := cmd.Run(); err != nil {
		os.Remove(outFile)
		return nil, err
	}
	defer os.Remove(outFile)

	data, err := os.ReadFile(outFile)
	if err != nil {
		return nil, err
	}
	var a astJSON
	if err := json.Unmarshal(data, &a); err != nil {
		return nil, err
	}
	if len(a.Classes) == 0 && len(a.Functions) == 0 && len(a.Vars) == 0 {
		return nil, fmt.Errorf("no symbols found")
	}
	return &a, nil
}

func toNodes(a *astJSON) []Node {
	var nodes []Node
	for _, c := range a.Classes {
		var fields []Field
		for _, f := range c.Fields {
			fields = append(fields, Field{Name: f.Name, Typ: mapType(f.Type)})
		}
		nodes = append(nodes, Node{Kind: "type", Name: c.Name, Fields: fields})
	}
	for _, f := range a.Functions {
		var params []Param
		for _, p := range f.Params {
			params = append(params, Param{Name: p.Name, Typ: mapType(p.Type)})
		}
		nodes = append(nodes, Node{Kind: "func", Name: f.Name, Params: params, Ret: mapType(f.Ret)})
	}
	for _, v := range a.Vars {
		nodes = append(nodes, Node{Kind: "var", Name: v.Name, Ret: mapType(v.Type)})
	}
	return nodes
}

// ConvertSource converts parsed nodes to Mochi source code.
func ConvertSource(nodes []Node, orig string) (string, error) {
	var b strings.Builder
	b.Write(meta.Header("//"))
	if orig != "" {
		b.WriteString("/*\n")
		b.WriteString(orig)
		if !strings.HasSuffix(orig, "\n") {
			b.WriteByte('\n')
		}
		b.WriteString("*/\n")
	}
	for _, n := range nodes {
		switch n.Kind {
		case "var":
			writeVar(&b, n)
		case "func":
			writeFunc(&b, n)
		case "type":
			writeType(&b, n)
		}
	}
	return b.String(), nil
}

// Convert converts parsed nodes to a Mochi AST node.
func Convert(nodes []Node) (*ast.Node, error) {
	src, err := ConvertSource(nodes, "")
	if err != nil {
		return nil, err
	}
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	return ast.FromProgram(prog), nil
}

func writeVar(b *strings.Builder, n Node) {
	b.WriteString("let ")
	b.WriteString(n.Name)
	if n.Ret != "" {
		b.WriteString(": ")
		b.WriteString(n.Ret)
	}
	b.WriteByte('\n')
}

func writeFunc(b *strings.Builder, n Node) {
	b.WriteString("fun ")
	b.WriteString(n.Name)
	writeFuncSignature(b, n)
}

func writeFuncSignature(b *strings.Builder, n Node) {
	b.WriteByte('(')
	for i, p := range n.Params {
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
	if n.Ret != "" && n.Ret != "Unit" {
		b.WriteString(": ")
		b.WriteString(n.Ret)
	}
	b.WriteString(" {\n}\n")
}

func writeType(b *strings.Builder, n Node) {
	b.WriteString("type ")
	b.WriteString(n.Name)
	b.WriteString(" {\n")
	for _, f := range n.Fields {
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

func mapType(t string) string {
	t = strings.TrimSpace(t)
	if strings.HasSuffix(t, "?") {
		t = strings.TrimSuffix(t, "?")
	}
	if strings.Contains(t, "->") {
		parts := strings.SplitN(t, "->", 2)
		params := strings.TrimSpace(parts[0])
		ret := strings.TrimSpace(parts[1])
		if strings.HasPrefix(params, "(") && strings.HasSuffix(params, ")") {
			params = params[1 : len(params)-1]
		}
		var mapped []string
		if params != "" {
			for _, p := range splitGeneric(params) {
				mp := mapType(p)
				if mp == "" {
					mp = "any"
				}
				mapped = append(mapped, mp)
			}
		}
		mret := mapType(ret)
		res := "fun(" + strings.Join(mapped, ", ") + ")"
		if mret != "" {
			res += ": " + mret
		}
		return res
	}
	if strings.HasPrefix(t, "Function") && strings.Contains(t, "<") && strings.HasSuffix(t, ">") {
		inner := t[strings.Index(t, "<")+1 : len(t)-1]
		parts := splitGeneric(inner)
		if len(parts) > 0 {
			ret := mapType(parts[len(parts)-1])
			var params []string
			for _, p := range parts[:len(parts)-1] {
				mp := mapType(p)
				if mp == "" {
					mp = "any"
				}
				params = append(params, mp)
			}
			res := "fun(" + strings.Join(params, ", ") + ")"
			if ret != "" {
				res += ": " + ret
			}
			return res
		}
	}
	switch t {
	case "", "Unit", "Nothing":
		return ""
	case "Int", "Long", "Short", "Byte":
		return "int"
	case "Float", "Double":
		return "float"
	case "Boolean":
		return "bool"
	case "String", "Char":
		return "string"
	}
	if strings.HasPrefix(t, "List<") && strings.HasSuffix(t, ">") {
		inner := mapType(t[5 : len(t)-1])
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasPrefix(t, "Set<") && strings.HasSuffix(t, ">") {
		inner := mapType(t[4 : len(t)-1])
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasPrefix(t, "Array<") && strings.HasSuffix(t, ">") {
		inner := mapType(t[6 : len(t)-1])
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasPrefix(t, "MutableList<") && strings.HasSuffix(t, ">") {
		inner := mapType(t[12 : len(t)-1])
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasPrefix(t, "MutableSet<") && strings.HasSuffix(t, ">") {
		inner := mapType(t[11 : len(t)-1])
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasPrefix(t, "Map<") && strings.HasSuffix(t, ">") {
		inner := t[4 : len(t)-1]
		parts := splitGeneric(inner)
		key := "any"
		val := "any"
		if len(parts) == 2 {
			if k := mapType(parts[0]); k != "" {
				key = k
			}
			if v := mapType(parts[1]); v != "" {
				val = v
			}
		}
		return "map<" + key + ", " + val + ">"
	}
	if strings.HasPrefix(t, "MutableMap<") && strings.HasSuffix(t, ">") {
		inner := t[11 : len(t)-1]
		parts := splitGeneric(inner)
		key := "any"
		val := "any"
		if len(parts) == 2 {
			if k := mapType(parts[0]); k != "" {
				key = k
			}
			if v := mapType(parts[1]); v != "" {
				val = v
			}
		}
		return "map<" + key + ", " + val + ">"
	}
	if strings.HasPrefix(t, "Pair<") && strings.HasSuffix(t, ">") {
		inner := t[5 : len(t)-1]
		parts := splitGeneric(inner)
		first := "any"
		second := "any"
		if len(parts) == 2 {
			if f := mapType(parts[0]); f != "" {
				first = f
			}
			if s := mapType(parts[1]); s != "" {
				second = s
			}
		}
		return "tuple<" + first + ", " + second + ">"
	}
	return t
}

func splitGeneric(s string) []string {
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
