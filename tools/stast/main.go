package main

import (
	"encoding/json"
	"fmt"
	"io"
	"os"
	"regexp"
	"strings"
)

type Program struct {
	Statements []Stmt `json:"statements"`
}

type Stmt struct {
	Kind string `json:"kind"`
	Name string `json:"name,omitempty"`
	Expr string `json:"expr,omitempty"`
	Cond string `json:"cond,omitempty"`
	Body []Stmt `json:"body,omitempty"`
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "usage: stast <file>")
		os.Exit(1)
	}
	var data []byte
	var err error
	if os.Args[1] == "-" {
		data, err = io.ReadAll(os.Stdin)
	} else {
		data, err = os.ReadFile(os.Args[1])
	}
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	prog := parse(string(data))
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	if err := enc.Encode(prog); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

func parse(src string) Program {
	lines := strings.Split(src, "\n")
	start := -1
	for i, l := range lines {
		if strings.HasPrefix(strings.TrimSpace(l), "!!") {
			start = i + 1
			break
		}
	}
	if start == -1 {
		start = 0
	}
	var stmts []Stmt
	for i := start; i < len(lines); i++ {
		l := strings.TrimSpace(lines[i])
		if l == "" || l == "." {
			continue
		}
		s := parseSimpleStmt(strings.TrimSuffix(l, "."))
		if s.Kind != "" {
			stmts = append(stmts, s)
		}
	}
	return Program{Statements: stmts}
}

func parseSimpleStmt(l string) Stmt {
	l = strings.TrimSpace(l)
	if strings.HasSuffix(l, "Transcript cr") {
		l = strings.TrimSuffix(l, "Transcript cr")
		l = strings.TrimSpace(strings.TrimSuffix(l, "."))
	}
	if strings.Contains(l, "displayOn: Transcript") {
		parts := strings.Split(l, "displayOn: Transcript")
		expr := strings.TrimSpace(parts[0])
		expr = strings.Trim(expr, "()")
		expr = strings.ReplaceAll(expr, "'", "\"")
		return Stmt{Kind: "print", Expr: expr}
	}
	if strings.Contains(l, ":=") {
		parts := strings.SplitN(l, ":=", 2)
		left := strings.TrimSpace(parts[0])
		right := strings.TrimSpace(strings.TrimSuffix(parts[1], "."))
		return Stmt{Kind: "assign", Name: left, Expr: right}
	}
	if strings.HasPrefix(l, "^") {
		return Stmt{Kind: "return", Expr: strings.TrimSpace(strings.TrimPrefix(l, "^"))}
	}
	if m := regexp.MustCompile(`^Transcript\s+show:`).FindString(l); m != "" {
		expr := strings.TrimSpace(strings.TrimPrefix(l, m))
		expr = strings.TrimSuffix(expr, ".")
		expr = strings.Trim(expr, "()")
		expr = strings.ReplaceAll(expr, "'", "\"")
		return Stmt{Kind: "print", Expr: expr}
	}
	return Stmt{}
}
