package swift

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"time"
)

// Program represents a parsed Swift source file as produced by
// `swiftc -dump-ast -dump-ast-format json`.
// Program represents a parsed Swift source file including its full AST.
type Program struct {
	File *Node `json:"file"`
}

// Node represents a single Swift AST node.
// Only a subset of common fields are captured explicitly and all
// child nodes are recursively stored in Children.
type Node struct {
	Kind     string       `json:"kind"`
	Name     string       `json:"name,omitempty"`
	Type     string       `json:"type,omitempty"`
	Range    *offsetRange `json:"range,omitempty"`
	Children []*Node      `json:"children,omitempty"`
}

type declName struct {
	BaseName baseName `json:"base_name"`
}

type baseName struct {
	Name string `json:"name"`
}

type offsetRange struct {
	Start int `json:"start"`
	End   int `json:"end"`
}

// Inspect parses Swift source code using the Swift compiler and
// returns its AST as a Program.
func Inspect(src string) (*Program, error) {
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
		return nil, fmt.Errorf("swiftc: %v: %s", err, out.String())
	}
	data := filterOutput(out.Bytes())
	var raw json.RawMessage
	if err := json.Unmarshal(data, &raw); err != nil {
		return nil, err
	}
	node, err := parseNode(raw)
	if err != nil {
		return nil, err
	}
	return &Program{File: node}, nil
}

func filterOutput(data []byte) []byte {
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
	return data
}

func parseNode(data json.RawMessage) (*Node, error) {
	var m map[string]json.RawMessage
	if err := json.Unmarshal(data, &m); err != nil {
		return nil, err
	}
	kindVal, ok := m["_kind"]
	if !ok {
		return nil, fmt.Errorf("missing _kind")
	}
	var n Node
	if err := json.Unmarshal(kindVal, &n.Kind); err != nil {
		return nil, err
	}
	delete(m, "_kind")
	for k, v := range m {
		switch k {
		case "name":
			var dn declName
			if err := json.Unmarshal(v, &dn); err == nil {
				n.Name = dn.BaseName.Name
			}
		case "type":
			_ = json.Unmarshal(v, &n.Type)
		case "range":
			var r offsetRange
			if err := json.Unmarshal(v, &r); err == nil {
				n.Range = &r
			}
		default:
			trimmed := bytes.TrimSpace(v)
			if len(trimmed) == 0 {
				continue
			}
			if trimmed[0] == '{' {
				if child, err := parseNode(v); err == nil {
					n.Children = append(n.Children, child)
				}
			} else if trimmed[0] == '[' {
				var arr []json.RawMessage
				if err := json.Unmarshal(v, &arr); err == nil {
					for _, a := range arr {
						if child, err := parseNode(a); err == nil {
							n.Children = append(n.Children, child)
						}
					}
				}
			}
		}
	}
	return &n, nil
}
