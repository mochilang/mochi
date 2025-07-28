//go:build slow

package java

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"
	"time"

	mochias "mochi/ast"
	mochiparser "mochi/parser"
)

// Node represents the simplified Java AST produced by the helper parser.
type Node struct {
	Body []Stmt `json:"body"`
}

// Stmt describes a statement in the simplified AST.
type Stmt struct {
	Kind  string `json:"kind"`
	Name  string `json:"name,omitempty"`
	Expr  *Expr  `json:"expr,omitempty"`
	Start *Expr  `json:"start,omitempty"`
	End   *Expr  `json:"end,omitempty"`
	Body  []Stmt `json:"body,omitempty"`
	Then  []Stmt `json:"then,omitempty"`
	Else  []Stmt `json:"else,omitempty"`
}

// Expr describes an expression in the simplified AST.
type Expr struct {
	Kind   string `json:"kind"`
	Value  string `json:"value,omitempty"`
	Name   string `json:"name,omitempty"`
	Left   *Expr  `json:"left,omitempty"`
	Right  *Expr  `json:"right,omitempty"`
	Target *Expr  `json:"target,omitempty"`
	Args   []Expr `json:"args,omitempty"`
	Expr   *Expr  `json:"expr,omitempty"`
	Cond   *Expr  `json:"cond,omitempty"`
	Then   *Expr  `json:"then,omitempty"`
	Else   *Expr  `json:"else,omitempty"`
	Elems  []Expr `json:"elems,omitempty"`
	Op     string `json:"op,omitempty"`
}

var (
	compileOnce sync.Once
	compileErr  error
	classDir    string
)

// ensureCompiled compiles the Java helper parser if needed.
func ensureCompiled() error {
	compileOnce.Do(func() {
		root := repoRoot()
		if root == "" {
			compileErr = fmt.Errorf("repo root not found")
			return
		}
		src := filepath.Join(root, "tools/a2mochi/x/java/internal/AstJson.java")
		classDir = filepath.Join(root, "tools/a2mochi/x/java/internal/bin")
		if _, err := os.Stat(filepath.Join(classDir, "AstJson.class")); err == nil {
			return
		}
		os.MkdirAll(classDir, 0o755)
		cmd := exec.Command("javac", "-d", classDir, src)
		if out, err := cmd.CombinedOutput(); err != nil {
			compileErr = fmt.Errorf("javac: %v: %s", err, out)
		}
	})
	return compileErr
}

// Parse converts Java source into a Node by invoking the helper Java parser.
func Parse(src string) (*Node, error) {
	tmp, err := os.CreateTemp("", "a2mochi-java-*.java")
	if err != nil {
		return nil, err
	}
	defer os.Remove(tmp.Name())
	if _, err := tmp.WriteString(src); err != nil {
		return nil, err
	}
	tmp.Close()
	return ParseFile(tmp.Name())
}

// ParseFile parses a Java file and returns a Node.
func ParseFile(path string) (*Node, error) {
	if err := ensureCompiled(); err != nil {
		return nil, err
	}
	cmd := exec.Command("java", "-cp", classDir, "internal.AstJson", path)
	out, err := cmd.Output()
	if err != nil {
		if ee, ok := err.(*exec.ExitError); ok {
			return nil, fmt.Errorf("java error: %s", string(ee.Stderr))
		}
		return nil, err
	}
	var n Node
	if err := json.Unmarshal(out, &n); err != nil {
		return nil, err
	}
	return &n, nil
}

// ConvertSource converts a Node into Mochi source code.
func ConvertSource(n *Node) (string, error) {
	var b strings.Builder
	b.WriteString(header())
	for _, st := range n.Body {
		emitStmt(&b, "", st)
	}
	return b.String(), nil
}

func emitStmt(b *strings.Builder, indent string, st Stmt) {
	switch st.Kind {
	case "VarDecl":
		b.WriteString(indent + "var " + st.Name)
		if st.Expr != nil {
			b.WriteString(" = ")
			emitExpr(b, *st.Expr)
		}
		b.WriteByte('\n')
	case "Assign":
		b.WriteString(indent + st.Name + " = ")
		if st.Expr != nil {
			emitExpr(b, *st.Expr)
		}
		b.WriteByte('\n')
	case "Print":
		b.WriteString(indent + "print(")
		if st.Expr != nil {
			emitExpr(b, *st.Expr)
		}
		b.WriteString(")\n")
	case "ForRange":
		b.WriteString(indent + "for " + st.Name + " in ")
		emitExpr(b, *st.Start)
		b.WriteString("..")
		emitExpr(b, *st.End)
		b.WriteString(" {\n")
		for _, s := range st.Body {
			emitStmt(b, indent+"  ", s)
		}
		b.WriteString(indent + "}\n")
	case "ForEach":
		b.WriteString(indent + "for " + st.Name + " in ")
		if st.Expr != nil {
			emitExpr(b, *st.Expr)
		}
		b.WriteString(" {\n")
		for _, s := range st.Body {
			emitStmt(b, indent+"  ", s)
		}
		b.WriteString(indent + "}\n")
	case "While":
		b.WriteString(indent + "while (")
		if st.Expr != nil {
			emitExpr(b, *st.Expr)
		}
		b.WriteString(") {\n")
		for _, s := range st.Body {
			emitStmt(b, indent+"  ", s)
		}
		b.WriteString(indent + "}\n")
	case "If":
		b.WriteString(indent + "if ")
		if st.Expr != nil {
			emitExpr(b, *st.Expr)
		}
		b.WriteString(" {\n")
		for _, s := range st.Then {
			emitStmt(b, indent+"  ", s)
		}
		if len(st.Else) > 0 {
			b.WriteString(indent + "} else {\n")
			for _, s := range st.Else {
				emitStmt(b, indent+"  ", s)
			}
		}
		b.WriteString(indent + "}\n")
	}
}

func emitExpr(b *strings.Builder, e Expr) {
	switch e.Kind {
	case "Literal":
		b.WriteString(e.Value)
	case "String":
		b.WriteString("\"")
		b.WriteString(e.Value)
		b.WriteString("\"")
	case "Ident":
		b.WriteString(e.Name)
	case "Binary":
		emitExpr(b, *e.Left)
		b.WriteString(" " + op(e.Op) + " ")
		emitExpr(b, *e.Right)
	case "Call":
		emitExpr(b, *e.Target)
		b.WriteString("(")
		for i, a := range e.Args {
			if i > 0 {
				b.WriteString(", ")
			}
			emitExpr(b, a)
		}
		b.WriteString(")")
	case "Member":
		emitExpr(b, *e.Expr)
		b.WriteString("." + e.Name)
	case "Array":
		b.WriteString("[")
		for i, el := range e.Elems {
			if i > 0 {
				b.WriteString(",")
			}
			emitExpr(b, el)
		}
		b.WriteString("]")
	case "Cond":
		if e.Then != nil && e.Else != nil &&
			e.Then.Kind == "Literal" && e.Then.Value == "1" &&
			e.Else.Kind == "Literal" && e.Else.Value == "0" {
			emitExpr(b, *e.Cond)
		} else {
			emitExpr(b, *e.Cond)
		}
	}
}

func op(k string) string {
	switch k {
	case "PLUS":
		return "+"
	case "MINUS":
		return "-"
	case "MULTIPLY":
		return "*"
	case "DIVIDE":
		return "/"
	case "REMAINDER":
		return "%"
	case "LESS_THAN":
		return "<"
	case "GREATER_THAN":
		return ">"
	case "EQUAL_TO":
		return "=="
	case "NOT_EQUAL_TO":
		return "!="
	case "LESS_THAN_EQUAL":
		return "<="
	case "GREATER_THAN_EQUAL":
		return ">="
	}
	return k
}

// Convert converts a parsed Node into a Mochi AST node.
func Convert(n *Node) (*mochias.Node, error) {
	src, err := ConvertSource(n)
	if err != nil {
		return nil, err
	}
	prog, err := mochiparser.ParseString(src)
	if err != nil {
		return nil, err
	}
	return mochias.FromProgram(prog), nil
}

// ConvertFile reads a Java file and converts it to a Mochi AST node.
func ConvertFile(path string) (*mochias.Node, error) {
	n, err := ParseFile(path)
	if err != nil {
		return nil, err
	}
	return Convert(n)
}

// ConvertFileSource reads a Java file and returns the generated Mochi source.
func ConvertFileSource(path string) (string, error) {
	n, err := ParseFile(path)
	if err != nil {
		return "", err
	}
	return ConvertSource(n)
}

func header() string {
	tz := time.FixedZone("GMT+7", 7*3600)
	t := time.Now().In(tz)
	return fmt.Sprintf("// Generated by a2mochi v%s on %s\n", version(), t.Format("2006-01-02 15:04:05 MST"))
}

func version() string {
	root := repoRoot()
	if root == "" {
		return "dev"
	}
	data, err := os.ReadFile(filepath.Join(root, "VERSION"))
	if err != nil {
		return "dev"
	}
	return strings.TrimSpace(string(data))
}

func repoRoot() string {
	dir, err := os.Getwd()
	if err != nil {
		return ""
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return ""
}
