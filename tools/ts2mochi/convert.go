package ts2mochi

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

// Program describes a simplified TypeScript program parsed from AST.
type Program struct {
	Funcs []Func     `json:"funcs"`
	Calls []CallExpr `json:"calls"`
}

type Func struct {
	Name   string   `json:"name"`
	Params []string `json:"params"`
	Body   []Stmt   `json:"body"`
}

type Stmt interface{}

type ReturnStmt struct{ Expr Expr }

type PrintStmt struct{ Expr Expr }

type Expr interface{}

type NumberLit struct{ Val string }

type Ident struct{ Name string }

type CallExpr struct {
	Name string `json:"name"`
	Args []Expr `json:"args"`
}

type ArrayLit struct {
	Elems []Expr `json:"elems"`
}

type rawProgram struct {
	Funcs []struct {
		Name   string    `json:"name"`
		Params []string  `json:"params"`
		Body   []rawStmt `json:"body"`
	} `json:"funcs"`
	Calls []struct {
		Name string `json:"name"`
	} `json:"calls"`
}

type rawStmt struct {
	Kind string          `json:"kind"`
	Expr json.RawMessage `json:"expr"`
}

type rawExpr struct {
	Kind  string    `json:"kind"`
	Value string    `json:"value"`
	Name  string    `json:"name"`
	Args  []rawExpr `json:"args"`
	Elems []rawExpr `json:"elems"`
}

// ConvertFile parses the TypeScript program at path and returns Mochi source.
// ConvertFile parses the TypeScript program at path and returns the generated
// Mochi source code.
func ConvertFile(path string) ([]byte, error) {
	code, _, err := ConvertFileWithJSON(path)
	return code, err
}

// ConvertFileWithJSON parses the TypeScript program and also returns the parsed
// AST in JSON form. The JSON is pretty-printed for readability.
func ConvertFileWithJSON(path string) ([]byte, []byte, error) {
	prog, astJSON, err := parseProgram(path)
	if err != nil {
		return nil, nil, err
	}
	code := formatProgram(prog)
	return code, astJSON, nil
}

func parseProgram(path string) (*Program, []byte, error) {
	root, err := repoRoot()
	if err != nil {
		return nil, nil, err
	}
	script := filepath.Join(root, "tools", "ts2mochi", "ast.js")
	cmd := exec.Command("node", script, path)
	if np := nodeModulePath(); np != "" {
		cmd.Env = append(os.Environ(), "NODE_PATH="+np)
	}
	var out bytes.Buffer
	var stderr bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		msg := strings.TrimSpace(stderr.String())
		if msg == "" {
			msg = err.Error()
		}
		return nil, nil, fmt.Errorf("node: %s", msg)
	}
	var rp rawProgram
	if err := json.Unmarshal(out.Bytes(), &rp); err != nil {
		return nil, nil, err
	}
	pretty, _ := json.MarshalIndent(&rp, "", "  ")
	return convertRawProgram(&rp), pretty, nil
}

func repoRoot() (string, error) {
	dir, err := filepath.Abs(".")
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
	return "", fmt.Errorf("go.mod not found")
}

func convertRawProgram(rp *rawProgram) *Program {
	p := &Program{}
	for _, f := range rp.Funcs {
		fn := Func{Name: f.Name, Params: f.Params}
		for _, b := range f.Body {
			switch b.Kind {
			case "return":
				fn.Body = append(fn.Body, ReturnStmt{convertRawExpr(b.Expr)})
			case "print":
				fn.Body = append(fn.Body, PrintStmt{convertRawExpr(b.Expr)})
			}
		}
		p.Funcs = append(p.Funcs, fn)
	}
	for _, c := range rp.Calls {
		p.Calls = append(p.Calls, CallExpr{Name: c.Name})
	}
	return p
}

func convertRawExpr(data json.RawMessage) Expr {
	if data == nil {
		return nil
	}
	var r rawExpr
	if err := json.Unmarshal(data, &r); err != nil {
		return nil
	}
	switch r.Kind {
	case "number":
		return NumberLit{Val: r.Value}
	case "ident":
		return Ident{Name: r.Name}
	case "call":
		var args []Expr
		for _, a := range r.Args {
			bdata, _ := json.Marshal(a)
			args = append(args, convertRawExpr(bdata))
		}
		return CallExpr{Name: r.Name, Args: args}
	case "array":
		var elems []Expr
		for _, el := range r.Elems {
			bdata, _ := json.Marshal(el)
			elems = append(elems, convertRawExpr(bdata))
		}
		return ArrayLit{Elems: elems}
	default:
		return Ident{Name: "unknown"}
	}
}

func formatProgram(p *Program) []byte {
	var out strings.Builder
	for _, fn := range p.Funcs {
		if fn.Name == "main" {
			continue
		}
		out.WriteString("fun " + fn.Name + "(")
		for i, n := range fn.Params {
			if i > 0 {
				out.WriteString(", ")
			}
			out.WriteString(n + ": int")
		}
		out.WriteString(")")
		out.WriteString(": int {\n")
		for _, st := range fn.Body {
			switch s := st.(type) {
			case ReturnStmt:
				out.WriteString("  return " + formatExpr(s.Expr) + "\n")
			case PrintStmt:
				out.WriteString("  print(" + formatExpr(s.Expr) + ")\n")
			}
		}
		out.WriteString("}\n\n")
	}
	for _, fn := range p.Funcs {
		if fn.Name == "main" {
			for _, st := range fn.Body {
				if ps, ok := st.(PrintStmt); ok {
					out.WriteString("print(" + formatExpr(ps.Expr) + ")")
				}
			}
			break
		}
	}
	out.WriteByte('\n')
	return []byte(out.String())
}

func formatExpr(e Expr) string {
	switch v := e.(type) {
	case NumberLit:
		return v.Val
	case Ident:
		return v.Name
	case CallExpr:
		var args []string
		for _, a := range v.Args {
			args = append(args, formatExpr(a))
		}
		return fmt.Sprintf("%s(%s)", v.Name, strings.Join(args, ","))
	case ArrayLit:
		var elems []string
		for _, e := range v.Elems {
			elems = append(elems, formatExpr(e))
		}
		return "[" + strings.Join(elems, ",") + "]"
	default:
		return ""
	}
}

func nodeModulePath() string {
	cmd := exec.Command("npm", "root", "-g")
	out, err := cmd.Output()
	if err != nil {
		return ""
	}
	return strings.TrimSpace(string(out))
}
