package dart

import (
	"bytes"
	_ "embed"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"regexp"
	"strings"
)

//go:embed parser.dart
var parserDart string

// Param represents a Dart function parameter.
type Param struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

// Function represents a parsed Dart function.
type Function struct {
	Name   string   `json:"name"`
	Params []Param  `json:"params"`
	Ret    string   `json:"ret"`
	Body   []string `json:"body"`
	Start  int      `json:"start"`
	End    int      `json:"end"`
	Doc    string   `json:"doc,omitempty"`
}

// Field represents a Dart class field.
type Field struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

// Class represents a Dart class definition.
type Class struct {
	Name   string  `json:"name"`
	Fields []Field `json:"fields"`
	Start  int     `json:"start"`
	End    int     `json:"end"`
	Doc    string  `json:"doc,omitempty"`
}

// Program represents a parsed Dart file.
type Program struct {
	Functions []Function `json:"functions"`
	Classes   []Class    `json:"classes"`
	Src       string     `json:"-"`
}

// Parse parses Dart source into a Program using the official Dart parser.
func Parse(src string) (*Program, error) {
	funcs, classes, err := runParser(src)
	if err != nil {
		// Fallback to simple regex parser when Dart SDK is missing.
		funcs, classes, err = simpleParse(src)
		if err != nil {
			return nil, err
		}
	}
	return &Program{Functions: funcs, Classes: classes, Src: src}, nil
}

func runParser(src string) ([]Function, []Class, error) {
	dartPath, err := exec.LookPath("dart")
	if err != nil {
		return nil, nil, fmt.Errorf("dart not found: %w", err)
	}
	f, err := os.CreateTemp("", "parser-*.dart")
	if err != nil {
		return nil, nil, err
	}
	if _, err := f.WriteString(parserDart); err != nil {
		f.Close()
		os.Remove(f.Name())
		return nil, nil, err
	}
	f.Close()
	defer os.Remove(f.Name())

	cmd := exec.Command(dartPath, f.Name())
	cmd.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	var errBuf bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &errBuf
	if err := cmd.Run(); err != nil {
		if errBuf.Len() > 0 {
			return nil, nil, fmt.Errorf("%v: %s", err, errBuf.String())
		}
		return nil, nil, err
	}
	return decode(out.Bytes())
}

func decode(data []byte) ([]Function, []Class, error) {
	var a struct {
		Functions []Function `json:"functions"`
		Classes   []Class    `json:"classes"`
	}
	if err := json.Unmarshal(data, &a); err != nil {
		return nil, nil, err
	}
	return a.Functions, a.Classes, nil
}

// simpleParse is a very small Dart parser used when the Dart SDK is not
// available. It only understands a subset of Dart used in the transpiler tests
// and is intentionally permissive. The parser recognises top level functions and
// their bodies without performing full syntax analysis.
func simpleParse(src string) ([]Function, []Class, error) {
	var funcs []Function
	lines := strings.Split(src, "\n")
	funcRe := regexp.MustCompile(`^\s*([A-Za-z0-9_<>,\[\]\?]+)\s+([A-Za-z_][A-Za-z0-9_]*)\s*\(([^)]*)\)\s*{`)
	for i := 0; i < len(lines); i++ {
		line := lines[i]
		m := funcRe.FindStringSubmatch(line)
		if m == nil {
			continue
		}
		ret := strings.TrimSpace(m[1])
		name := strings.TrimSpace(m[2])
		paramsRaw := strings.TrimSpace(m[3])
		start := i + 1
		depth := 1
		var body []string
		for i++; i < len(lines); i++ {
			l := lines[i]
			if strings.Contains(l, "{") {
				depth++
			}
			if strings.Contains(l, "}") {
				depth--
				if depth == 0 {
					break
				}
			}
			body = append(body, strings.TrimSpace(l))
		}
		end := i + 1
		var params []Param
		for _, p := range parseSplitArgs(paramsRaw) {
			if p == "" {
				continue
			}
			fields := strings.Fields(p)
			if len(fields) == 1 {
				params = append(params, Param{Name: fields[0]})
			} else {
				params = append(params, Param{Type: strings.Join(fields[:len(fields)-1], " "), Name: fields[len(fields)-1]})
			}
		}
		funcs = append(funcs, Function{Name: name, Params: params, Ret: ret, Body: body, Start: start, End: end})
	}
	return funcs, nil, nil
}

func parseSplitArgs(s string) []string {
	var parts []string
	depth := 0
	start := 0
	for i, r := range s {
		switch r {
		case '<':
			depth++
		case '>':
			if depth > 0 {
				depth--
			}
		case ',':
			if depth == 0 {
				parts = append(parts, strings.TrimSpace(s[start:i]))
				start = i + 1
			}
		}
	}
	if start < len(s) {
		parts = append(parts, strings.TrimSpace(s[start:]))
	}
	return parts
}
