//go:build slow

package swift

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
)

// ConvertError provides detailed parsing errors from swiftc.
type ConvertError struct {
	Line   int
	Column int
	Msg    string
	Snip   string
}

func (e *ConvertError) Error() string {
	if e.Line > 0 {
		if e.Column > 0 {
			return fmt.Sprintf("line %d:%d: %s\n%s", e.Line, e.Column, e.Msg, e.Snip)
		}
		return fmt.Sprintf("line %d: %s\n%s", e.Line, e.Msg, e.Snip)
	}
	return fmt.Sprintf("%s\n%s", e.Msg, e.Snip)
}

// Program mirrors the JSON AST produced by swiftc -dump-ast.
type Program struct {
	Items []item `json:"items"`
	Src   string `json:"-"`
}

type item struct {
	Kind          string      `json:"_kind"`
	Name          *declName   `json:"name,omitempty"`
	Params        *paramList  `json:"params,omitempty"`
	Body          *body       `json:"body,omitempty"`
	Range         offsetRange `json:"range"`
	Result        string      `json:"result,omitempty"`
	InterfaceType string      `json:"interface_type,omitempty"`
	ThrownType    string      `json:"thrown_type,omitempty"`
	Access        string      `json:"access,omitempty"`
	Members       []item      `json:"members,omitempty"`
	Elements      []item      `json:"elements,omitempty"`
}

type declName struct {
	BaseName baseName `json:"base_name"`
}

type baseName struct {
	Name string `json:"name"`
}

type paramList struct {
	Params []param `json:"params"`
}

type param struct {
	Name          declName `json:"name"`
	InterfaceType string   `json:"interface_type,omitempty"`
}

type body struct {
	Range offsetRange `json:"range"`
}

type offsetRange struct {
	Start int `json:"start"`
	End   int `json:"end"`
}

// Parse runs swiftc to obtain the JSON AST for src.
func Parse(src string) (*Program, error) {
	tmp, err := os.CreateTemp("", "swift-src-*.swift")
	if err != nil {
		return nil, err
	}
	defer os.Remove(tmp.Name())
	if _, err := tmp.WriteString(src); err != nil {
		tmp.Close()
		return nil, err
	}
	tmp.Close()

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, "swiftc", "-dump-ast", "-dump-ast-format", "json", tmp.Name())
	var out bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &out
	if err := cmd.Run(); err != nil {
		line, col := 0, 0
		msg := err.Error()
		outStr := out.String()
		if idx := strings.Index(outStr, ": error: "); idx != -1 {
			prefix := outStr[:idx]
			parts := strings.Split(prefix, ":")
			if len(parts) >= 2 {
				line, _ = strconv.Atoi(parts[len(parts)-2])
				col, _ = strconv.Atoi(parts[len(parts)-1])
			}
			msg = outStr[idx+len(": error: "):]
			if nl := strings.IndexByte(msg, '\n'); nl != -1 {
				msg = msg[:nl]
			}
		}
		return nil, &ConvertError{Line: line, Column: col, Msg: msg, Snip: snippetAround(src, line, col)}
	}
	data := out.Bytes()
	var filtered [][]byte
	for _, line := range bytes.Split(data, []byte("\n")) {
		trim := bytes.TrimSpace(line)
		if len(trim) == 0 {
			continue
		}
		if trim[0] >= '0' && trim[0] <= '9' && bytes.Contains(trim, []byte("|")) {
			continue
		}
		if bytes.HasPrefix(trim, []byte("tests/")) && bytes.Contains(trim, []byte("warning")) {
			continue
		}
		filtered = append(filtered, trim)
	}
	data = bytes.Join(filtered, []byte("\n"))
	start := bytes.Index(data, []byte("{\"_kind\""))
	if start >= 0 {
		data = data[start:]
		depth := 0
		inStr := false
		for i, b := range data {
			switch b {
			case '"':
				if i == 0 || data[i-1] != '\\' {
					inStr = !inStr
				}
			case '{':
				if !inStr {
					depth++
				}
			case '}':
				if !inStr {
					depth--
					if depth == 0 {
						data = data[:i+1]
						break
					}
				}
			}
		}
	}
	dec := json.NewDecoder(bytes.NewReader(data))
	var f Program
	if err := dec.Decode(&f); err != nil {
		return nil, fmt.Errorf("decode AST: %w", err)
	}
	f.Src = src
	return &f, nil
}

// ParseFile reads a Swift file and parses it.
func ParseFile(path string) (*Program, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Parse(string(data))
}

func snippet(src string) string {
	lines := strings.Split(src, "\n")
	if len(lines) > 10 {
		lines = lines[:10]
	}
	for i, l := range lines {
		lines[i] = fmt.Sprintf("%3d: %s", i+1, l)
	}
	return strings.Join(lines, "\n")
}

func snippetAround(src string, line, col int) string {
	lines := strings.Split(src, "\n")
	if line <= 0 || line > len(lines) {
		return snippet(src)
	}
	start := line - 2
	if start < 0 {
		start = 0
	}
	end := line + 2
	if end > len(lines) {
		end = len(lines)
	}
	var out strings.Builder
	for i := start; i < end; i++ {
		fmt.Fprintf(&out, "%4d| %s\n", i+1, lines[i])
		if i == line-1 {
			caretPos := len(lines[i])
			if col > 0 && col-1 < caretPos {
				caretPos = col - 1
			}
			out.WriteString("    | " + strings.Repeat(" ", caretPos) + "^\n")
		}
	}
	return strings.TrimRight(out.String(), "\n")
}

func repoRoot() string {
	dir, _ := os.Getwd()
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

func version() string {
	root := repoRoot()
	if root == "" {
		return "dev"
	}
	if data, err := os.ReadFile(filepath.Join(root, "VERSION")); err == nil {
		return strings.TrimSpace(string(data))
	}
	return "dev"
}
