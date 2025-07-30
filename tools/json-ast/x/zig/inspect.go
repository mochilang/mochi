package zig

import (
	"bytes"
	"context"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"time"
)

// zigCmd is the name or path to the Zig executable. It can be overridden
// in tests via the ZIG environment variable.
var zigCmd = "zig"

// Program represents a parsed Zig source file.
type Program struct {
	Vars      []Variable `json:"vars"`
	Structs   []Struct   `json:"structs"`
	Functions []Function `json:"functions"`
}

type Variable struct {
	Name  string `json:"name"`
	Type  string `json:"type"`
	Value string `json:"value,omitempty"`
	Const bool   `json:"const,omitempty"`
	Pub   bool   `json:"pub,omitempty"`
	Line  int    `json:"line"`
}

type Function struct {
	Name    string   `json:"name"`
	Params  string   `json:"params"`
	Ret     string   `json:"ret"`
	Pub     bool     `json:"pub,omitempty"`
	Line    int      `json:"line"`
	EndLine int      `json:"endLine"`
	Lines   []string `json:"lines"`
}

type Struct struct {
	Name    string  `json:"name"`
	Pub     bool    `json:"pub,omitempty"`
	Line    int     `json:"line"`
	EndLine int     `json:"endLine"`
	Fields  []Field `json:"fields"`
}

type Field struct {
	Name string `json:"name"`
	Type string `json:"type"`
	Line int    `json:"line"`
}

// Inspect parses the given Zig source code and returns its program structure.
func Inspect(src string) (*Program, error) {
	if env := os.Getenv("ZIG"); env != "" {
		zigCmd = env
	}
	if _, err := exec.LookPath(zigCmd); err != nil {
		return nil, fmt.Errorf("zig not installed")
	}
	tmp, err := os.CreateTemp("", "zigsrc_*.zig")
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

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	cmd := exec.CommandContext(ctx, zigCmd, "ast-check", tmp.Name())
	var errBuf bytes.Buffer
	cmd.Stderr = &errBuf
	if err := cmd.Run(); err != nil {
		if msg := strings.TrimSpace(errBuf.String()); msg != "" {
			return nil, fmt.Errorf("%v: %s", err, msg)
		}
		return nil, err
	}
	// The current stable Zig compiler does not provide a JSON AST output.
	// For now we simply verify that the source parses without errors and
	// return an empty Program structure.
	return &Program{}, nil
}
