package rb

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"strconv"
	"strings"
)

// Node represents a Ruby AST node produced by ripper.
type Node struct {
	Type     string
	Value    string
	Line     int
	Col      int
	Children []Node
	EndLine  int
	EndCol   int
	Source   string
}

// Parse converts Ruby source code into a Node tree using ripper.
func Parse(src string) (*Node, error) {
	if _, err := exec.LookPath("ruby"); err != nil {
		return nil, fmt.Errorf("ruby not installed: %w", err)
	}
	cmd := exec.Command("ruby", "-e", `require 'json';require 'ripper';src=STDIN.read;b=Ripper::SexpBuilder.new(src);ast=b.parse;if b.error?; STDERR.puts b.error; exit 1; end;puts JSON.generate(ast)`)
	cmd.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	var errBuf bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &errBuf
	if err := cmd.Run(); err != nil {
		if errBuf.Len() > 0 {
			msg := strings.TrimSpace(errBuf.String())
			line := 0
			for i := 0; i < len(msg); i++ {
				if msg[i] < '0' || msg[i] > '9' {
					continue
				}
				j := i
				for j < len(msg) && msg[j] >= '0' && msg[j] <= '9' {
					j++
				}
				if j < len(msg) && msg[j] == ':' {
					if n, err := strconv.Atoi(msg[i:j]); err == nil {
						line = n
					}
					break
				}
				i = j
			}
			if line > 0 {
				snip := snippetAround(src, line)
				return nil, fmt.Errorf("line %d: %s\n%s", line, msg, snip)
			}
			return nil, fmt.Errorf("%s", msg)
		}
		return nil, err
	}
	var data any
	if err := json.Unmarshal(out.Bytes(), &data); err != nil {
		return nil, err
	}
	node := buildNode(data)
	node.Source = src
	return &node, nil
}

// ParseFile reads Ruby source code from a file and parses it using Parse.
func ParseFile(path string) (*Node, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Parse(string(data))
}

func buildNode(v any) Node {
	if s, ok := v.(string); ok {
		return Node{Type: "@tok", Value: s}
	}
	arr, ok := v.([]any)
	if !ok || len(arr) == 0 {
		return Node{}
	}
	typeStr, ok := arr[0].(string)
	if !ok {
		n := Node{Type: "list"}
		for _, x := range arr {
			c := buildNode(x)
			if c.Type != "" {
				n.Children = append(n.Children, c)
			}
		}
		return n
	}
	n := Node{Type: typeStr}
	if strings.HasPrefix(n.Type, "@") {
		if len(arr) > 1 {
			if s, ok := arr[1].(string); ok {
				n.Value = s
			}
		}
		if len(arr) > 2 {
			if pos, ok := arr[2].([]any); ok && len(pos) >= 2 {
				if ln, ok := pos[0].(float64); ok {
					n.Line = int(ln)
				}
				if col, ok := pos[1].(float64); ok {
					n.Col = int(col)
				}
			}
		}
		n.EndLine = n.Line
		n.EndCol = n.Col
		return n
	}
	for i := 1; i < len(arr); i++ {
		child := buildNode(arr[i])
		if n.Line == 0 && child.Line != 0 {
			n.Line = child.Line
			n.Col = child.Col
		}
		if child.Type != "" {
			n.Children = append(n.Children, child)
			n.EndLine = child.EndLine
			n.EndCol = child.EndCol
		}
	}
	return n
}

func snippetAround(src string, line int) string {
	lines := strings.Split(src, "\n")
	start := line - 2
	if start < 0 {
		start = 0
	}
	end := line
	if end >= len(lines) {
		end = len(lines) - 1
	}
	for i := start; i <= end; i++ {
		prefix := "   "
		if i+1 == line {
			prefix = ">>>"
		}
		lines[i] = fmt.Sprintf("%s %d: %s", prefix, i+1, lines[i])
	}
	return strings.Join(lines[start:end+1], "\n")
}
