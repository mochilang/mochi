package c

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"
	"time"

	"mochi/ast"
	"mochi/parser"
)

type Node struct {
	Kind  string `json:"kind"`
	Name  string `json:"name"`
	Value string `json:"value"`
	Inner []Node `json:"inner"`
	Ref   *Node  `json:"referencedDecl"`
}

var version = func() string {
	_, file, _, ok := runtime.Caller(0)
	if ok {
		data, err := os.ReadFile(filepath.Join(filepath.Dir(file), "../../../VERSION"))
		if err == nil {
			return strings.TrimSpace(string(data))
		}
	}
	return "dev"
}()

func fileHeader() string {
	tz := time.FixedZone("GMT+7", 7*3600)
	return fmt.Sprintf("// a2mochi c v%s %s GMT+7", version, time.Now().In(tz).Format("2006-01-02 15:04:05"))
}

func runClangAST(src string) (*Node, error) {
	cmd := exec.Command("clang", "-w", "-x", "c", "-", "-Xclang", "-ast-dump=json", "-fsyntax-only")
	cmd.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &out
	if err := cmd.Run(); err != nil {
		return nil, fmt.Errorf("clang failed: %v", err)
	}
	data := out.Bytes()
	var root Node
	if err := json.Unmarshal(data[jsonIndex(data):], &root); err != nil {
		return nil, err
	}
	return &root, nil
}

// DebugParse is exported for testing. It parses C source and returns the AST root.
func DebugParse(src string) (*Node, error) { return runClangAST(src) }

// Parse parses C source code using clang's AST dump.
func Parse(src string) (*Node, error) { return runClangAST(src) }

func jsonIndex(b []byte) int {
	for i, c := range b {
		if c == '{' {
			return i
		}
	}
	return 0
}

func ConvertSource(src string) (string, error) {
	root, err := Parse(src)
	if err != nil {
		return "", err
	}
	prints := findPrints(root)
	var b strings.Builder
	b.WriteString(fileHeader())
	b.WriteByte('\n')
	b.WriteString("/*\n")
	b.WriteString(src)
	if !strings.HasSuffix(src, "\n") {
		b.WriteByte('\n')
	}
	b.WriteString("*/\n")
	for _, p := range prints {
		b.WriteString("print(")
		b.WriteString(p)
		b.WriteString(")\n")
	}
	return b.String(), nil
}

func Convert(src string) (*ast.Node, error) {
	code, err := ConvertSource(src)
	if err != nil {
		return nil, err
	}
	prog, err := parser.ParseString(code)
	if err != nil {
		return nil, err
	}
	return ast.FromProgram(prog), nil
}

// ConvertSourceFromNode converts a parsed C AST node into Mochi source
// code with the standard header attached.
func ConvertSourceFromNode(src string, n *Node) (string, error) {
	if n == nil {
		return "", fmt.Errorf("nil C AST")
	}
	prints := findPrints(n)
	var b strings.Builder
	b.WriteString(fileHeader())
	b.WriteByte('\n')
	b.WriteString("/*\n")
	b.WriteString(src)
	if !strings.HasSuffix(src, "\n") {
		b.WriteByte('\n')
	}
	b.WriteString("*/\n")
	for _, p := range prints {
		b.WriteString("print(")
		b.WriteString(p)
		b.WriteString(")\n")
	}
	return b.String(), nil
}

// ConvertFromNode converts an already parsed C AST node to a Mochi AST node.
func ConvertFromNode(src string, n *Node) (*ast.Node, error) {
	code, err := ConvertSourceFromNode(src, n)
	if err != nil {
		return nil, err
	}
	prog, err := parser.ParseString(code)
	if err != nil {
		return nil, err
	}
	return ast.FromProgram(prog), nil
}

func findPrints(n *Node) []string {
	var prints []string
	if n.Kind == "FunctionDecl" && n.Name == "main" {
		for i := range n.Inner {
			c := n.Inner[i]
			if c.Kind == "CompoundStmt" {
				prints = append(prints, extractPrints(&c)...)
			}
		}
	}
	for i := range n.Inner {
		prints = append(prints, findPrints(&n.Inner[i])...)
	}
	return prints
}

// DebugFindPrints exposes findPrints for debugging.
func DebugFindPrints(n *Node) []string { return findPrints(n) }

func extractPrints(n *Node) []string {
	var out []string
	for i := range n.Inner {
		c := n.Inner[i]
		if c.Kind == "CallExpr" {
			if callee := calleeName(&c); callee == "printf" || callee == "puts" {
				if arg := firstValue(&c); arg != "" {
					out = append(out, arg)
				}
			}
		}
	}
	return out
}

func calleeName(n *Node) string {
	if len(n.Inner) == 0 {
		return ""
	}
	return walkName(&n.Inner[0])
}

func walkName(n *Node) string {
	if n.Kind == "DeclRefExpr" {
		if n.Name != "" {
			return n.Name
		}
		if n.Ref != nil && n.Ref.Name != "" {
			return n.Ref.Name
		}
	}
	if len(n.Inner) > 0 {
		return walkName(&n.Inner[0])
	}
	return ""
}

func firstValue(n *Node) string {
	start := 1
	if len(n.Inner) > 2 {
		start = 2
	}
	for i := start; i < len(n.Inner); i++ {
		if v := valueOf(&n.Inner[i]); v != "" {
			return v
		}
	}
	return ""
}

func valueOf(n *Node) string {
	switch n.Kind {
	case "IntegerLiteral", "FloatingLiteral":
		return n.Value
	case "StringLiteral":
		return strconv.Quote(strings.Trim(n.Value, "\""))
	}
	for i := range n.Inner {
		if v := valueOf(&n.Inner[i]); v != "" {
			return v
		}
	}
	return ""
}
