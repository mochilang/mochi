package c

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os/exec"
	"regexp"
	"strconv"
	"strings"
	"time"
)

// clangNode represents a subset of Clang's JSON AST.
type clangNode struct {
	Kind       string      `json:"kind"`
	Name       string      `json:"name"`
	Type       *clangType  `json:"type"`
	IsImplicit *bool       `json:"isImplicit"`
	Range      clangRange  `json:"range"`
	Inner      []clangNode `json:"inner"`
}

type clangType struct {
	QualType string `json:"qualType"`
}

type clangRange struct {
	Begin clangPos `json:"begin"`
	End   clangPos `json:"end"`
}

type clangPos struct {
	Offset int `json:"offset"`
}

func offsetToLine(src string, off int) int {
	if off < 0 {
		return 0
	}
	line := 1
	for i := 0; i < off && i < len(src); i++ {
		if src[i] == '\n' {
			line++
		}
	}
	return line
}

// parseCFileClang parses C source code using clang's JSON AST output.
func parseClangFile(src string) ([]function, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, "clang", "-w", "-x", "c", "-", "-Xclang", "-ast-dump=json", "-fsyntax-only")
	cmd.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	var errBuf bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &errBuf
	if err := cmd.Run(); err != nil {
		if errBuf.Len() > 0 {
			return nil, fmt.Errorf("%s", formatClangErrors(src, errBuf.String()))
		}
		return nil, err
	}
	data := out.Bytes()
	if idx := bytes.IndexByte(data, '{'); idx > 0 {
		data = data[idx:]
	}
	var root clangNode
	if err := json.Unmarshal(data, &root); err != nil {
		return nil, err
	}
	var funcs []function
	var walk func(n clangNode)
	walk = func(n clangNode) {
		if n.Kind == "FunctionDecl" && (n.IsImplicit == nil || !*n.IsImplicit) {
			if strings.Contains(n.Name, "_create") {
				return
			}
			ret := ""
			if n.Type != nil {
				typ := n.Type.QualType
				if open := strings.Index(typ, "("); open != -1 {
					ret = mapType(strings.TrimSpace(typ[:open]))
				} else {
					ret = mapType(typ)
				}
			}
			var params []param
			var body []string
			startLn := offsetToLine(src, n.Range.Begin.Offset)
			endLn := offsetToLine(src, n.Range.End.Offset)
			for _, c := range n.Inner {
				switch c.Kind {
				case "ParmVarDecl":
					name := c.Name
					t := ""
					if c.Type != nil {
						t = mapType(c.Type.QualType)
					}
					params = append(params, param{name: name, typ: t})
				case "CompoundStmt":
					start := c.Range.Begin.Offset
					end := c.Range.End.Offset
					if start >= 0 && end <= len(src) && end > start {
						body = parseStatements(src[start+1 : end])
					}
				}
			}
			if len(body) > 0 {
				srcLines := strings.Split(src, "\n")
				if startLn-1 >= 0 && endLn <= len(srcLines) {
					snippet := strings.Join(srcLines[startLn-1:endLn], "\n")
					funcs = append(funcs, function{name: n.Name, ret: ret, params: params, body: body, startLine: startLn, endLine: endLn, source: snippet})
				} else {
					funcs = append(funcs, function{name: n.Name, ret: ret, params: params, body: body, startLine: startLn, endLine: endLn})
				}
			}
		}
		for _, c := range n.Inner {
			walk(c)
		}
	}
	walk(root)
	return funcs, nil
}

// formatClangErrors converts clang error output into a friendlier form with
// line numbers and code snippets. If parsing fails, the raw text is returned.
func formatClangErrors(src, out string) string {
	re := regexp.MustCompile(`(?m)^<stdin>:(\d+):(\d+):.*error: (.*)$`)
	matches := re.FindAllStringSubmatch(out, -1)
	if len(matches) == 0 {
		return strings.TrimSpace(out)
	}
	lines := strings.Split(src, "\n")
	var buf strings.Builder
	for _, m := range matches {
		ln, _ := strconv.Atoi(m[1])
		col, _ := strconv.Atoi(m[2])
		msg := m[3]
		buf.WriteString(fmt.Sprintf("line %d:%d: %s\n", ln, col, msg))
		start := ln - 2
		if start < 0 {
			start = 0
		}
		end := ln + 1
		if end >= len(lines) {
			end = len(lines) - 1
		}
		for i := start; i <= end && i < len(lines); i++ {
			buf.WriteString(fmt.Sprintf("%4d| %s\n", i+1, lines[i]))
			if i == ln-1 {
				buf.WriteString("     " + strings.Repeat(" ", col-1) + "^\n")
			}
		}
	}
	return strings.TrimSpace(buf.String())
}
