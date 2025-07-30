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
type Program struct {
	Items []item `json:"items"`
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
	var prog Program
	if err := json.Unmarshal(data, &prog); err != nil {
		return nil, err
	}
	return &prog, nil
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
