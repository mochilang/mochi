//go:build slow

package php

import (
	"fmt"
	"os"
	"strings"

	"mochi/ast"
	transpmeta "mochi/transpiler/meta"
)

// Print returns Mochi source code for the given AST node.
func Print(node *ast.Node) (string, error) {
	var b strings.Builder
	if err := ast.Fprint(&b, node); err != nil {
		return "", err
	}
	out := b.String()
	out = strings.ReplaceAll(out, "((", "(")
	out = strings.ReplaceAll(out, "))", ")")
	return out, nil
}

// ConvertFile reads a PHP file and converts it to Mochi code with a header.
func ConvertFile(path string) (string, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return "", err
	}
	prog, err := Parse(string(data))
	if err != nil {
		return "", formatParseError(path, string(data), err)
	}
	node, err := Transform(prog)
	if err != nil {
		return "", formatError(path, string(data), err)
	}
	out, err := Print(node)
	if err != nil {
		return "", err
	}
	var b strings.Builder
	b.Write(transpmeta.Header("//"))
	b.WriteString("/*\n")
	b.WriteString(string(data))
	if len(data) > 0 && data[len(data)-1] != '\n' {
		b.WriteByte('\n')
	}
	b.WriteString("*/\n")
	b.WriteString(out)
	result := b.String()
	if len(result) > 0 && result[len(result)-1] != '\n' {
		result += "\n"
	}
	return result, nil
}

func snippet(src string) string {
	lines := strings.Split(src, "\n")
	if len(lines) > 15 {
		lines = lines[:15]
	}
	for i, l := range lines {
		lines[i] = fmt.Sprintf("%3d: %s", i+1, l)
	}
	return strings.Join(lines, "\n")
}

func formatParseError(path, src string, err error) error {
	if e, ok := err.(*ParseError); ok {
		sn := arrowSnippet(src, e.Line)
		return &ConvertError{Path: path, Line: e.Line, Msg: e.Msg, Snip: sn}
	}
	return &ConvertError{Path: path, Msg: err.Error(), Snip: snippet(src)}
}

func formatError(path, src string, err error) error {
	if conv, ok := err.(*ConvertError); ok {
		if conv.Path == "" {
			conv.Path = path
		}
		return conv
	}
	if e, ok := err.(*ParseError); ok {
		sn := arrowSnippet(src, e.Line)
		return &ConvertError{Path: path, Line: e.Line, Msg: e.Msg, Snip: sn}
	}
	if err.Error() == "no convertible symbols found" {
		return &ConvertError{Path: path, Msg: err.Error(), Snip: snippet(src)}
	}
	return &ConvertError{Path: path, Msg: err.Error()}
}

func arrowSnippet(src string, line int) string {
	lines := strings.Split(src, "\n")
	start := line - 3
	if start < 0 {
		start = 0
	}
	end := line + 2
	if end > len(lines) {
		end = len(lines)
	}
	var b strings.Builder
	for i := start; i < end; i++ {
		mark := "   "
		if i+1 == line {
			mark = ">>>"
		}
		fmt.Fprintf(&b, "%4d:%s %s\n", i+1, mark, lines[i])
	}
	return b.String()
}
