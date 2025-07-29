//go:build slow

package zig

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
)

// Program represents the parsed Zig AST in JSON form along with the original source.
type Program struct {
	JSON map[string]any
	Src  string
}

// Parse invokes the Zig compiler to obtain a JSON AST for src.
func Parse(src string) (*Program, error) {
	if _, err := exec.LookPath("zig"); err != nil {
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

	cmd := exec.Command("zig", "ast-check", "--format", "json", tmp.Name())
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Run(); err != nil {
		return nil, err
	}
	var data map[string]any
	if err := json.Unmarshal(out.Bytes(), &data); err != nil {
		return nil, err
	}
	return &Program{JSON: data, Src: src}, nil
}
