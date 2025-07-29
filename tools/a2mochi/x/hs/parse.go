//go:build slow

package hs

import (
	"bytes"
	_ "embed"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
)

//go:embed parse.hs
var parseScript string

// Field describes a record field in a data declaration.
type Field struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

// Item represents a minimal Haskell AST item parsed from source.
type Item struct {
	Kind       string   `json:"kind"`
	Name       string   `json:"name,omitempty"`
	Params     []string `json:"params,omitempty"`
	Body       string   `json:"body,omitempty"`
	Type       string   `json:"type,omitempty"`
	Fields     []Field  `json:"fields,omitempty"`
	Collection string   `json:"collection,omitempty"`
	Start      string   `json:"start,omitempty"`
	End        string   `json:"end,omitempty"`
	Line       int      `json:"line,omitempty"`
}

// Program is the top-level structure returned by Parse.
type Program struct {
	Items  []Item `json:"items"`
	Source string `json:"-"`
}

// Parse invokes the embedded Haskell parser script to produce a Program.
func Parse(src string) (*Program, error) {
	if _, err := exec.LookPath("runghc"); err != nil {
		return nil, fmt.Errorf("runghc not installed")
	}
	tmp, err := os.CreateTemp("", "a2mochi-*.hs")
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

	script, err := os.CreateTemp("", "parse-*.hs")
	if err != nil {
		return nil, err
	}
	if _, err := script.WriteString(parseScript); err != nil {
		script.Close()
		os.Remove(script.Name())
		return nil, err
	}
	script.Close()
	defer os.Remove(script.Name())

	cmd := exec.Command("runghc", script.Name(), tmp.Name())
	var out bytes.Buffer
	var errBuf bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &errBuf
	if err := cmd.Run(); err != nil {
		if errBuf.Len() > 0 {
			return nil, fmt.Errorf("%v: %s", err, errBuf.String())
		}
		return nil, err
	}

	var items []Item
	if err := json.Unmarshal(out.Bytes(), &items); err != nil {
		return nil, err
	}
	return &Program{Items: items, Source: src}, nil
}
