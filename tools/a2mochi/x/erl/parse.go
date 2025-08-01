//go:build slow

package erl

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"
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

// Program is the parsed representation of an Erlang file.
type Program struct {
	Module    string   `json:"module"`
	Functions []Func   `json:"functions"`
	Records   []Record `json:"records"`
}

// Parse parses Erlang source code using the bundled escript parser.
func Parse(src string) (*Program, error) {
	return parseProgram(src)
}

// ParseFile reads an Erlang file and returns its parsed representation.
func ParseFile(path string) (*Program, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return parseProgram(string(data))
}

func parseProgram(src string) (*Program, error) {
	if _, err := exec.LookPath("escript"); err != nil {
		return nil, fmt.Errorf("escript not found")
	}
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
	if custom := os.Getenv("ERL_PARSER"); custom != "" {
		script = custom
	}
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
	var res Program
	if err := json.Unmarshal(out.Bytes(), &res); err != nil {
		return nil, err
	}
	return &res, nil
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
