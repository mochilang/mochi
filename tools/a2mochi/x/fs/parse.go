//go:build slow

package fs

// Package fs implements a small F# parser for the any2mochi tool. It relies on
// the official F# compiler service via a small helper script to obtain a JSON
// representation of the source.

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	_ "embed"
)

//go:embed parse.fsx
var parseScript string

type Var struct {
	Name    string `json:"name"`
	Expr    string `json:"expr"`
	Mutable bool   `json:"mutable"`
	Type    string `json:"type"`
	Line    int    `json:"line"`
	Raw     string `json:"raw"`
}

type Assign struct {
	Name  string `json:"name"`
	Index string `json:"index"`
	Expr  string `json:"expr"`
	Line  int    `json:"line"`
	Raw   string `json:"raw"`
}

type ForRange struct {
	Var   string `json:"var"`
	Start string `json:"start"`
	End   string `json:"end"`
	Body  []Stmt `json:"body"`
	Line  int    `json:"line"`
	Raw   string `json:"raw"`
}

type ForIn struct {
	Var  string `json:"var"`
	Expr string `json:"expr"`
	Body []Stmt `json:"body"`
	Line int    `json:"line"`
	Raw  string `json:"raw"`
}

type While struct {
	Cond string `json:"cond"`
	Body []Stmt `json:"body"`
	Line int    `json:"line"`
	Raw  string `json:"raw"`
}

type Print struct {
	Expr string `json:"expr"`
	Line int    `json:"line"`
	Raw  string `json:"raw"`
}

type Stmt interface{}

// Fun represents a simple function declaration parsed from the generated F# code.
type Fun struct {
	Name   string  `json:"name"`
	Params []Field `json:"params"`
	Ret    string  `json:"ret"`
	Body   []Stmt  `json:"body"`
	Line   int     `json:"line"`
	Raw    string  `json:"raw"`
}

// Return models a return statement which originates from a raise expression.
type Return struct {
	Expr string `json:"expr"`
	Line int    `json:"line"`
	Raw  string `json:"raw"`
}

// Break represents a loop break statement.
type Break struct {
	Line int    `json:"line"`
	Raw  string `json:"raw"`
}

// Continue represents a loop continue statement.
type Continue struct {
	Line int    `json:"line"`
	Raw  string `json:"raw"`
}

// If represents a simple if-else statement.
type If struct {
	Cond string `json:"cond"`
	Then []Stmt `json:"then"`
	Else []Stmt `json:"else"`
	Line int    `json:"line"`
	Raw  string `json:"raw"`
}

type Field struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

type Variant struct {
	Name   string  `json:"name"`
	Fields []Field `json:"fields"`
}

type TypeDecl struct {
	Name     string    `json:"name"`
	Fields   []Field   `json:"fields"`
	Variants []Variant `json:"variants"`
	Line     int       `json:"line"`
	Raw      string    `json:"raw"`
}

type Expect struct {
	Cond string `json:"cond"`
	Line int    `json:"line"`
	Raw  string `json:"raw"`
}

type Program struct {
	// Source holds the original F# input. It is preserved so the
	// converter can emit the original program as a block comment.
	Source string   `json:"source"`
	Vars   []Var    `json:"vars"`
	Prints []string `json:"prints"`
	Stmts  []Stmt   `json:"stmts"`
}

// Parse invokes a small F# script that uses the official compiler service to
// parse the provided source and return a JSON representation compatible with
// the Program structure. The script is executed via `dotnet fsi`. If the
// command fails the error is forwarded to the caller.
func Parse(src string) (*Program, error) {
	tmp, err := os.CreateTemp("", "parse-*.fsx")
	if err != nil {
		return nil, err
	}
	if _, err := tmp.WriteString(parseScript); err != nil {
		tmp.Close()
		os.Remove(tmp.Name())
		return nil, err
	}
	tmp.Close()
	defer os.Remove(tmp.Name())

	cmd := exec.Command("dotnet", "fsi", tmp.Name())
	cmd.Stdin = strings.NewReader(src)
	out, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("dotnet fsi failed: %w", err)
	}
	var p Program
	if err := json.Unmarshal(out, &p); err != nil {
		return nil, err
	}
	p.Source = src
	return &p, nil
}

// ParseFile reads the F# source at path and returns the parsed Program.
func ParseFile(path string) (*Program, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Parse(string(data))
}
