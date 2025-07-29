//go:build slow

package ex

import (
	"bytes"
	_ "embed"
	"encoding/json"
	"fmt"
	"os/exec"
	"strings"
)

//go:embed parse.exs
var elixirParser string

// RawAST represents the Elixir AST as generic JSON.
type RawAST interface{}

// ParseAST parses Elixir source using the official parser and returns the raw AST in JSON form.
func ParseAST(src string) (RawAST, error) {
	if _, err := exec.LookPath("elixir"); err != nil {
		return nil, fmt.Errorf("elixir not installed")
	}
	cmd := exec.Command("elixir", "-e", elixirParser)
	cmd.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	var errBuf bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &errBuf
	if err := cmd.Run(); err != nil {
		return nil, fmt.Errorf("elixir parse error: %v: %s", err, errBuf.String())
	}
	var ast RawAST
	if err := json.Unmarshal(out.Bytes(), &ast); err != nil {
		return nil, err
	}
	return ast, nil
}

var skipFuncs = map[string]bool{
	"_index_string": true,
	"_slice_string": true,
	"_count":        true,
	"_sum":          true,
	"_avg":          true,
	"_union":        true,
	"_except":       true,
	"_intersect":    true,
	"_group_by":     true,
	"_query":        true,
}

type Func struct {
	Name      string
	Params    []string
	Body      []string
	StartLine int
	EndLine   int
	Header    string
	Doc       string
	Comments  []string
	Raw       []string
}

type Program struct {
	Funcs  []Func
	Module string
	Source string
}

type ConvertError struct {
	Line int
	Msg  string
	Snip string
}

func (e *ConvertError) Error() string {
	if e.Line > 0 {
		return fmt.Sprintf("line %d: %s\n%s", e.Line, e.Msg, e.Snip)
	}
	return fmt.Sprintf("%s\n%s", e.Msg, e.Snip)
}

func newConvertError(line int, lines []string, msg string) error {
	start := line - 2
	if start < 0 {
		start = 0
	}
	end := line
	if end >= len(lines) {
		end = len(lines) - 1
	}
	var b strings.Builder
	for i := start; i <= end; i++ {
		prefix := "   "
		if i+1 == line {
			prefix = ">>>"
		}
		fmt.Fprintf(&b, "%s %d: %s\n", prefix, i+1, strings.TrimRight(lines[i], "\n"))
	}
	return &ConvertError{Line: line, Msg: msg, Snip: strings.TrimRight(b.String(), "\n")}
}

func Parse(src string) (*Program, error) {
	raw, err := ParseAST(src)
	if err != nil {
		return nil, err
	}
	lines := strings.Split(src, "\n")
	prog := &Program{Source: src}
	prog.Module = findModuleName(raw)
	collectFuncs(raw, lines, &prog.Funcs)
	if len(prog.Funcs) == 0 {
		return nil, newConvertError(1, lines, "no functions found")
	}
	return prog, nil
}

// findModuleName searches the raw AST for a defmodule declaration and returns
// its name if present.
func findModuleName(n interface{}) string {
	arr, ok := n.([]interface{})
	if !ok {
		if m, ok := n.(map[string]interface{}); ok {
			for _, v := range m {
				if name := findModuleName(v); name != "" {
					return name
				}
			}
		}
		return ""
	}
	if len(arr) >= 3 {
		if op, ok := arr[0].(string); ok && op == "defmodule" {
			modNode, ok := arr[2].([]interface{})
			if ok && len(modNode) > 0 {
				if alias, ok := modNode[0].([]interface{}); ok && len(alias) >= 3 {
					names, _ := alias[2].([]interface{})
					var parts []string
					for _, p := range names {
						if s, ok := p.(string); ok {
							parts = append(parts, s)
						}
					}
					return strings.Join(parts, ".")
				}
			}
		}
	}
	for _, v := range arr {
		if name := findModuleName(v); name != "" {
			return name
		}
	}
	return ""
}

// extractLine looks for a "line" entry inside metadata and returns it if found.
func extractLine(meta []interface{}) int {
	for _, e := range meta {
		if pair, ok := e.([]interface{}); ok && len(pair) == 2 {
			if key, _ := pair[0].(string); key == "line" {
				if v, ok := pair[1].(float64); ok {
					return int(v)
				}
			}
		}
	}
	return 0
}

// maxLine recursively scans the AST node to find the largest line number.
func maxLine(n interface{}, cur int) int {
	switch x := n.(type) {
	case []interface{}:
		if len(x) > 1 {
			if meta, ok := x[1].([]interface{}); ok {
				if l := extractLine(meta); l > cur {
					cur = l
				}
			}
		}
		for _, v := range x {
			if l := maxLine(v, cur); l > cur {
				cur = l
			}
		}
	case map[string]interface{}:
		for _, v := range x {
			if l := maxLine(v, cur); l > cur {
				cur = l
			}
		}
	}
	return cur
}

// parseFunc builds a Func from a def AST node.
func parseFunc(n []interface{}, lines []string) *Func {
	if len(n) < 3 {
		return nil
	}
	pair, ok := n[2].([]interface{})
	if !ok || len(pair) < 2 {
		return nil
	}
	head, _ := pair[0].([]interface{})
	if len(head) < 3 {
		return nil
	}
	name, _ := head[0].(string)
	meta, _ := head[1].([]interface{})
	params, _ := head[2].([]interface{})
	fn := &Func{Name: name}
	fn.StartLine = extractLine(meta)
	for _, p := range params {
		if arr, ok := p.([]interface{}); ok && len(arr) > 0 {
			if s, ok := arr[0].(string); ok {
				fn.Params = append(fn.Params, s)
			}
		}
	}
	if fn.StartLine > 0 {
		seg, end := captureFuncLines(lines, fn.StartLine)
		fn.Raw = seg
		fn.EndLine = end
		if fn.StartLine-1 < len(lines) {
			fn.Header = strings.TrimSpace(lines[fn.StartLine-1])
		}
	}
	return fn
}

func captureFuncLines(lines []string, start int) ([]string, int) {
	seg := []string{}
	for i := start - 1; i < len(lines); i++ {
		l := strings.TrimSpace(lines[i])
		seg = append(seg, l)
		if i > start-1 && l == "end" {
			return seg, i + 1
		}
	}
	return seg, len(lines)
}

// collectFuncs walks the raw AST and appends parsed functions to out.
func collectFuncs(n interface{}, lines []string, out *[]Func) {
	arr, ok := n.([]interface{})
	if !ok {
		if m, ok := n.(map[string]interface{}); ok {
			for _, v := range m {
				collectFuncs(v, lines, out)
			}
		}
		return
	}
	if len(arr) >= 3 {
		if op, ok := arr[0].(string); ok && (op == "def" || op == "defp") {
			if fn := parseFunc(arr, lines); fn != nil {
				if !skipFuncs[fn.Name] && !strings.HasPrefix(fn.Name, "_") {
					*out = append(*out, *fn)
				}
			}
			return
		}
	}
	for _, v := range arr {
		collectFuncs(v, lines, out)
	}
}
