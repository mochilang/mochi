package php

import (
	"fmt"
	"os"
	"strings"
)

// ConvertError represents a conversion failure with line context.
type ConvertError struct {
	Path string
	Line int
	Msg  string
	Snip string
}

func (e *ConvertError) Error() string {
	loc := e.Path
	if e.Line > 0 {
		loc = fmt.Sprintf("%s:%d", e.Path, e.Line)
	}
	return fmt.Sprintf("%s: %s\n%s", loc, e.Msg, e.Snip)
}

// Convert parses PHP source code and emits minimal Mochi stubs.
func Convert(src string) ([]byte, error) {
	prog, err := Parse(src)
	if err != nil {
		return nil, formatParseError("", src, err)
	}
	var out strings.Builder
	for _, c := range prog.Classes {
		if c.Doc != "" {
			out.WriteString("// ")
			out.WriteString(strings.ReplaceAll(c.Doc, "\n", " "))
			out.WriteByte('\n')
		}
		out.WriteString("type ")
		out.WriteString(c.Name)
		if len(c.Fields) == 0 && len(c.Methods) == 0 {
			out.WriteString(" {}\n")
		} else {
			out.WriteString(" {\n")
			for _, f := range c.Fields {
				out.WriteString("  ")
				out.WriteString(f.Name)
				if f.Type == "" {
					out.WriteString(": any\n")
				} else {
					out.WriteString(": ")
					out.WriteString(f.Type)
					out.WriteByte('\n')
				}
			}
			for _, m := range c.Methods {
				if m.Doc != "" {
					out.WriteString("  // ")
					out.WriteString(strings.ReplaceAll(m.Doc, "\n", " "))
					out.WriteByte('\n')
				}
				out.WriteString("  fun ")
				out.WriteString(m.Name)
				out.WriteByte('(')
				for i, p := range m.Params {
					if i > 0 {
						out.WriteString(", ")
					}
					out.WriteString(p.Name)
					out.WriteString(": ")
					if p.Type != "" {
						out.WriteString(p.Type)
					} else {
						out.WriteString("any")
					}
				}
				out.WriteString(")")
				out.WriteString(": ")
				if m.Return != "" {
					out.WriteString(m.Return)
				} else {
					out.WriteString("any")
				}
				out.WriteString(" {}\n")
			}
			out.WriteString("}\n")
		}
	}
	for _, f := range prog.Functions {
		if f.Doc != "" {
			out.WriteString("// ")
			out.WriteString(strings.ReplaceAll(f.Doc, "\n", " "))
			out.WriteByte('\n')
		}
		out.WriteString("fun ")
		out.WriteString(f.Name)
		out.WriteByte('(')
		for i, p := range f.Params {
			if i > 0 {
				out.WriteString(", ")
			}
			out.WriteString(p.Name)
			out.WriteString(": ")
			if p.Type != "" {
				out.WriteString(p.Type)
			} else {
				out.WriteString("any")
			}
		}
		out.WriteString(")")
		out.WriteString(": ")
		if f.Return != "" {
			out.WriteString(f.Return)
		} else {
			out.WriteString("any")
		}
		out.WriteString(" {}\n")
	}
	for _, v := range prog.Vars {
		out.WriteString("let ")
		out.WriteString(v.Name)
		if v.Value != "" {
			out.WriteString(" = ")
			out.WriteString(v.Value)
		}
		out.WriteByte('\n')
	}
	for _, p := range prog.Prints {
		out.WriteString("print(")
		out.WriteString(p.Expr)
		out.WriteString(")\n")
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found")
	}
	return []byte(out.String()), nil
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

// ConvertFile reads the php file and converts it to Mochi.
func ConvertFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	out, err := Convert(string(data))
	if err != nil {
		return nil, formatError(path, string(data), err)
	}
	return out, nil
}

func formatParseError(path, src string, err error) error {
	if e, ok := err.(*ParseError); ok {
		snippet := arrowSnippet(src, e.Line)
		return &ConvertError{Path: path, Line: e.Line, Msg: e.Msg, Snip: snippet}
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
		snippet := arrowSnippet(src, e.Line)
		return &ConvertError{Path: path, Line: e.Line, Msg: e.Msg, Snip: snippet}
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
