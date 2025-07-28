package erl

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	"mochi/ast"
	"mochi/parser"
)

// Func represents a top level Erlang function.
type Func struct {
	Name     string   `json:"name"`
	Params   []string `json:"params"`
	Body     []string `json:"body"`
	Line     int      `json:"line"`
	EndLine  int      `json:"end"`
	Arity    int      `json:"arity"`
	Exported bool     `json:"exported"`
}

// Record represents a simple Erlang record definition.
type Record struct {
	Name    string   `json:"name"`
	Fields  []string `json:"fields"`
	Line    int      `json:"line"`
	EndLine int      `json:"end,omitempty"`
}

// AST is the parsed representation of an Erlang file.
type AST struct {
	Module    string   `json:"module"`
	Functions []Func   `json:"functions"`
	Records   []Record `json:"records"`
}

// Parse parses Erlang source using the bundled escript parser.
func Parse(src string) (*AST, error) {
	if _, err := exec.LookPath("escript"); err != nil {
		return nil, fmt.Errorf("escript not found")
	}
	// drop shebang
	if strings.HasPrefix(src, "#!") {
		if i := strings.Index(src, "\n"); i != -1 {
			src = src[i+1:]
		} else {
			src = ""
		}
	}
	tmp, err := os.CreateTemp("", "src-*.erl")
	if err != nil {
		return nil, err
	}
	defer os.Remove(tmp.Name())
	if _, err := tmp.WriteString(src); err != nil {
		return nil, err
	}
	tmp.Close()

	root, err := repoRoot()
	if err != nil {
		return nil, err
	}
	script := filepath.Join(root, "archived", "tools", "any2mochi", "x", "erlang", "parser", "parser.escript")
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, "escript", script, tmp.Name())
	var out bytes.Buffer
	var stderr bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		var perr struct {
			Error string `json:"error"`
		}
		if jsonErr := json.Unmarshal(out.Bytes(), &perr); jsonErr == nil && perr.Error != "" {
			return nil, fmt.Errorf("parse error: %s", perr.Error)
		}
		if stderr.Len() > 0 {
			return nil, fmt.Errorf("%v: %s", err, strings.TrimSpace(stderr.String()))
		}
		return nil, err
	}
	var res AST
	if err := json.Unmarshal(out.Bytes(), &res); err != nil {
		return nil, err
	}
	return &res, nil
}

// ConvertSource converts a parsed AST into Mochi source code.
func ConvertSource(ast *AST) (string, error) {
	if ast == nil {
		return "", fmt.Errorf("nil ast")
	}
	var out strings.Builder
	if ast.Module != "" {
		out.WriteString("package ")
		out.WriteString(ast.Module)
		out.WriteString("\n\n")
	}
	for _, r := range ast.Records {
		out.WriteString("// line ")
		out.WriteString(fmt.Sprint(r.Line))
		if r.EndLine > 0 && r.EndLine != r.Line {
			out.WriteString("-")
			out.WriteString(fmt.Sprint(r.EndLine))
		}
		out.WriteByte('\n')
		out.WriteString("type ")
		out.WriteString(strings.Title(r.Name))
		out.WriteString(" {\n")
		for _, f := range r.Fields {
			out.WriteString("  ")
			out.WriteString(f)
			out.WriteString(": any\n")
		}
		out.WriteString("}\n")
	}
	hasMain := false
	for _, f := range ast.Functions {
		if f.Name == "main" {
			hasMain = true
		}
		if strings.HasPrefix(f.Name, "mochi_") && f.Name != "main" {
			continue
		}
		out.WriteString("// line ")
		out.WriteString(fmt.Sprint(f.Line))
		if f.EndLine > 0 && f.EndLine != f.Line {
			out.WriteString("-")
			out.WriteString(fmt.Sprint(f.EndLine))
		}
		if f.Exported {
			out.WriteString(" (exported)")
		}
		out.WriteByte('\n')
		out.WriteString("fun ")
		if f.Name == "" {
			out.WriteString("fun")
		} else {
			out.WriteString(f.Name)
		}
		out.WriteByte('(')
		params := f.Params
		if f.Name == "main" && len(params) == 1 && params[0] == "_" {
			params = nil
		}
		for i, p := range params {
			if i > 0 {
				out.WriteString(", ")
			}
			out.WriteString(p)
		}
		out.WriteByte(')')
		if len(f.Body) == 0 {
			out.WriteString(" {}\n")
		} else {
			out.WriteString(" {\n")
			for _, line := range f.Body {
				parts := strings.Split(line, "\n")
				for _, ln := range parts {
					ln = convertLine(ln, ast.Records)
					out.WriteString("  ")
					out.WriteString(strings.TrimSpace(ln))
					out.WriteByte('\n')
				}
			}
			out.WriteString("}\n")
		}
	}
	if hasMain {
		out.WriteString("main()\n")
	}
	if out.Len() == 0 {
		return "", fmt.Errorf("no convertible symbols found")
	}
	return out.String(), nil
}

// Convert parses the Mochi source produced by ConvertSource into an AST node.
func Convert(astFile *AST) (*ast.Node, error) {
	src, err := ConvertSource(astFile)
	if err != nil {
		return nil, err
	}
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	return ast.FromProgram(prog), nil
}

func convertLine(ln string, recs []Record) string {
	if strings.HasPrefix(ln, "io:format(") {
		ln = strings.TrimPrefix(ln, "io:format(")
		ln = rewritePrintCall(ln)
	} else if strings.HasPrefix(ln, "io:fwrite(") {
		ln = strings.TrimPrefix(ln, "io:fwrite(")
		ln = rewritePrintCall(ln)
	} else if strings.HasPrefix(ln, "mochi_print([") && strings.HasSuffix(ln, "])") {
		ln = "print(" + strings.TrimSuffix(strings.TrimPrefix(ln, "mochi_print(["), "])") + ")"
	}
	for _, r := range recs {
		t := strings.Title(r.Name)
		if strings.Contains(ln, "#"+r.Name+"{") {
			ln = strings.ReplaceAll(ln, "#"+r.Name+"{", t+" {")
			if i := strings.Index(ln, t+" {"); i != -1 {
				after := ln[i+len(t)+2:]
				after = strings.ReplaceAll(after, "=", ":")
				ln = ln[:i+len(t)+2] + after
			}
		}
		ln = strings.ReplaceAll(ln, "#"+r.Name+".", ".")
	}
	return ln
}

func rewritePrintCall(args string) string {
	if strings.HasPrefix(args, "\"~s~n\", [") && strings.HasSuffix(args, "])") {
		return "print(" + strings.TrimSuffix(strings.TrimPrefix(args, "\"~s~n\", ["), "])") + ")"
	}
	return "print(" + args
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

func formatParseError(src string, err error) error {
	msg := err.Error()
	line := 0
	if strings.HasPrefix(msg, "parse error: {") {
		parts := strings.SplitN(msg[len("parse error: "):], ",", 2)
		if len(parts) > 0 {
			n, _ := strconv.Atoi(strings.TrimLeft(strings.TrimSpace(parts[0]), "{"))
			line = n
		}
	}
	lines := strings.Split(src, "\n")
	if line > 0 && line <= len(lines) {
		start := line - 2
		if start < 0 {
			start = 0
		}
		end := line + 2
		if end >= len(lines) {
			end = len(lines) - 1
		}
		var b strings.Builder
		for i := start; i <= end; i++ {
			prefix := "   "
			if i+1 == line {
				prefix = ">>>"
			}
			fmt.Fprintf(&b, "%s %d: %s\n", prefix, i+1, lines[i])
			if i+1 == line {
				b.WriteString("    ^\n")
			}
		}
		return fmt.Errorf("line %d: %s\n%s", line, msg, strings.TrimRight(b.String(), "\n"))
	}
	return fmt.Errorf("%s", msg)
}
