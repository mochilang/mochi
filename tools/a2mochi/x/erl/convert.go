//go:build slow

package erl

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"

	"mochi/ast"
	"mochi/parser"
)

// Func represents a top level Erlang function.
type Func struct {
	Name     string   `json:"name"`
	Params   []string `json:"params"`
	Body     []string `json:"body"`
	Line     int      `json:"line"`
	EndLine  int      `json:"end"`
	Arity    int      `json:"arity"`
	Exported bool     `json:"exported"`
}

// Record represents a simple Erlang record definition.
type Record struct {
	Name    string   `json:"name"`
	Fields  []string `json:"fields"`
	Line    int      `json:"line"`
	EndLine int      `json:"end,omitempty"`
}

// AST is the parsed representation of an Erlang file.
type AST struct {
	Module    string   `json:"module"`
	Functions []Func   `json:"functions"`
	Records   []Record `json:"records"`
}

var assignRe = regexp.MustCompile(`^([A-Za-z_][A-Za-z0-9_]*)\s*=\s*(.+)$`)
var appendRe = regexp.MustCompile(`lists:append\(([^,]+),\s*\[([^\]]+)\]\)`)
var mapsGetRe = regexp.MustCompile(`maps:get\(([^,]+),\s*([^\)]+)\)`)
var mapsPutRe = regexp.MustCompile(`maps:put\(([^,]+),\s*([^,]+),\s*([^\)]+)\)`)
var mapsIsKeyRe = regexp.MustCompile(`maps:is_key\(([^,]+),\s*([^\)]+)\)`)

// Parse parses a very small subset of Erlang syntax and returns an AST.
// It only supports module attributes, exports and simple function bodies.
func Parse(src string) (*AST, error) {
	// drop shebang if present
	if strings.HasPrefix(src, "#!") {
		if i := strings.Index(src, "\n"); i != -1 {
			src = src[i+1:]
		} else {
			src = ""
		}
	}

	lines := strings.Split(strings.ReplaceAll(src, "\r\n", "\n"), "\n")
	astFile := &AST{}
	exported := map[string]int{}

	for i := 0; i < len(lines); i++ {
		raw := lines[i]
		line := strings.TrimSpace(raw)
		if strings.HasPrefix(line, "-module(") {
			if end := strings.Index(line, ")"); end != -1 {
				name := line[len("-module("):end]
				name = strings.Trim(name, "'\"")
				astFile.Module = name
			}
			continue
		}
		if strings.HasPrefix(line, "-export(") {
			if open := strings.Index(line, "["); open != -1 {
				if close := strings.Index(line[open+1:], "]"); close != -1 {
					list := line[open+1 : open+1+close]
					for _, item := range strings.Split(list, ",") {
						item = strings.TrimSpace(strings.TrimSuffix(item, "."))
						if item == "" {
							continue
						}
						parts := strings.Split(item, "/")
						if len(parts) != 2 {
							continue
						}
						arity, _ := strconv.Atoi(strings.TrimSpace(parts[1]))
						exported[strings.Trim(parts[0], "'\"")] = arity
					}
				}
			}
			continue
		}

		idx := strings.Index(line, "->")
		if idx == -1 {
			continue
		}
		header := strings.TrimSpace(line[:idx])
		after := strings.TrimSpace(line[idx+2:])
		open := strings.Index(header, "(")
		close := strings.LastIndex(header, ")")
		if open == -1 || close == -1 || close < open {
			continue
		}
		name := strings.TrimSpace(header[:open])
		paramsText := strings.TrimSpace(header[open+1 : close])
		var params []string
		if paramsText != "" {
			for _, p := range strings.Split(paramsText, ",") {
				params = append(params, strings.TrimSpace(p))
			}
		}
		fn := Func{
			Name:    name,
			Params:  params,
			Line:    i + 1,
			Arity:   len(params),
			EndLine: i + 1,
		}
		if ar, ok := exported[name]; ok && ar == len(params) {
			fn.Exported = true
		}
		if after != "" {
			if strings.HasSuffix(after, ".") {
				fn.Body = []string{strings.TrimSuffix(after, ".")}
			} else {
				fn.Body = []string{after}
				fn.EndLine = i + 1
			}
		}
		if !strings.HasSuffix(after, ".") {
			for j := i + 1; j < len(lines); j++ {
				ln := strings.TrimSpace(lines[j])
				if strings.HasSuffix(ln, ".") {
					fn.Body = append(fn.Body, strings.TrimSuffix(ln, "."))
					fn.EndLine = j + 1
					i = j
					break
				}
				fn.Body = append(fn.Body, ln)
				fn.EndLine = j + 1
			}
		}
		astFile.Functions = append(astFile.Functions, fn)
	}

	for i := range astFile.Functions {
		f := &astFile.Functions[i]
		if ar, ok := exported[f.Name]; ok && ar == f.Arity {
			f.Exported = true
		}
	}

	return astFile, nil
}

// ConvertSource converts a parsed AST into Mochi source code.
func ConvertSource(ast *AST) (string, error) {
	if ast == nil {
		return "", fmt.Errorf("nil ast")
	}
	var out strings.Builder
	if ast.Module != "" {
		out.WriteString("package ")
		out.WriteString(ast.Module)
		out.WriteString("\n\n")
	}
	for _, r := range ast.Records {
		out.WriteString("// line ")
		out.WriteString(fmt.Sprint(r.Line))
		if r.EndLine > 0 && r.EndLine != r.Line {
			out.WriteString("-")
			out.WriteString(fmt.Sprint(r.EndLine))
		}
		out.WriteByte('\n')
		out.WriteString("type ")
		out.WriteString(strings.Title(r.Name))
		out.WriteString(" {\n")
		for _, f := range r.Fields {
			out.WriteString("  ")
			out.WriteString(f)
			out.WriteString(": any\n")
		}
		out.WriteString("}\n")
	}
	hasMain := false
	for _, f := range ast.Functions {
		if f.Name == "main" {
			hasMain = true
		}
		if strings.HasPrefix(f.Name, "mochi_") && f.Name != "main" {
			continue
		}
		out.WriteString("// line ")
		out.WriteString(fmt.Sprint(f.Line))
		if f.EndLine > 0 && f.EndLine != f.Line {
			out.WriteString("-")
			out.WriteString(fmt.Sprint(f.EndLine))
		}
		if f.Exported {
			out.WriteString(" (exported)")
		}
		out.WriteByte('\n')
		out.WriteString("fun ")
		if f.Name == "" {
			out.WriteString("fun")
		} else {
			out.WriteString(f.Name)
		}
		out.WriteByte('(')
		params := f.Params
		if f.Name == "main" && len(params) == 1 && params[0] == "_" {
			params = nil
		}
		for i, p := range params {
			if i > 0 {
				out.WriteString(", ")
			}
			out.WriteString(p)
		}
		out.WriteByte(')')
		if len(f.Body) == 0 {
			out.WriteString(" {}\n")
		} else {
			out.WriteString(" {\n")
			for _, line := range f.Body {
				parts := strings.Split(line, "\n")
				for _, ln := range parts {
					ln = convertLine(ln, ast.Records)
					out.WriteString("  ")
					out.WriteString(strings.TrimSpace(ln))
					out.WriteByte('\n')
				}
			}
			out.WriteString("}\n")
		}
	}
	if hasMain {
		out.WriteString("main()\n")
	}
	if out.Len() == 0 {
		return "", fmt.Errorf("no convertible symbols found")
	}
	return out.String(), nil
}

// Convert parses the Mochi source produced by ConvertSource into an AST node.
func Convert(astFile *AST) (*ast.Node, error) {
	src, err := ConvertSource(astFile)
	if err != nil {
		return nil, err
	}
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	return ast.FromProgram(prog), nil
}

func convertLine(ln string, recs []Record) string {
	ln = strings.TrimSuffix(strings.TrimSpace(ln), ",")
	if strings.HasPrefix(ln, "io:format(") {
		ln = strings.TrimPrefix(ln, "io:format(")
		ln = rewritePrintCall(ln)
	} else if strings.HasPrefix(ln, "io:fwrite(") {
		ln = strings.TrimPrefix(ln, "io:fwrite(")
		ln = rewritePrintCall(ln)
	} else if strings.HasPrefix(ln, "mochi_print([") && strings.HasSuffix(ln, "])") {
		ln = "print(" + strings.TrimSuffix(strings.TrimPrefix(ln, "mochi_print(["), "])") + ")"
	}
	if appendRe.MatchString(ln) {
		ln = appendRe.ReplaceAllString(ln, "append($1, $2)")
	}
	if m := assignRe.FindStringSubmatch(strings.TrimSuffix(ln, ",")); m != nil {
		ln = "let " + m[1] + " = " + m[2]
	}
	if strings.Contains(ln, "lists:sum(") {
		ln = strings.ReplaceAll(ln, "lists:sum(", "sum(")
	}
	if strings.Contains(ln, "lists:min(") {
		ln = strings.ReplaceAll(ln, "lists:min(", "min(")
	}
	if strings.Contains(ln, "lists:max(") {
		ln = strings.ReplaceAll(ln, "lists:max(", "max(")
	}
	if strings.Contains(ln, "length(") {
		ln = strings.ReplaceAll(ln, "length(", "len(")
	}
	if strings.Contains(ln, "maps:size(") {
		ln = strings.ReplaceAll(ln, "maps:size(", "len(")
	}
	if strings.Contains(ln, "maps:values(") {
		ln = strings.ReplaceAll(ln, "maps:values(", "values(")
	}
	if strings.Contains(ln, "#{") {
		ln = strings.ReplaceAll(ln, "#{", "{")
		ln = strings.ReplaceAll(ln, "=>", ":")
	}
	if mapsGetRe.MatchString(ln) {
		ln = mapsGetRe.ReplaceAllString(ln, "$2[$1]")
	}
	if mapsPutRe.MatchString(ln) {
		ln = mapsPutRe.ReplaceAllString(ln, "$3[$1] = $2")
	}
	if mapsIsKeyRe.MatchString(ln) {
		ln = mapsIsKeyRe.ReplaceAllString(ln, "$1 in $2")
	}
	for _, r := range recs {
		t := strings.Title(r.Name)
		if strings.Contains(ln, "#"+r.Name+"{") {
			ln = strings.ReplaceAll(ln, "#"+r.Name+"{", t+" {")
			if i := strings.Index(ln, t+" {"); i != -1 {
				after := ln[i+len(t)+2:]
				after = strings.ReplaceAll(after, "=", ":")
				ln = ln[:i+len(t)+2] + after
			}
		}
		ln = strings.ReplaceAll(ln, "#"+r.Name+".", ".")
	}
	return ln
}

func rewritePrintCall(args string) string {
	if strings.HasPrefix(args, "\"~s~n\", [") && strings.HasSuffix(args, "])") {
		return "print(" + strings.TrimSuffix(strings.TrimPrefix(args, "\"~s~n\", ["), "])") + ")"
	}
	if strings.HasPrefix(args, "\"~p~n\", [") && strings.HasSuffix(args, "])") {
		return "print(" + strings.TrimSuffix(strings.TrimPrefix(args, "\"~p~n\", ["), "])") + ")"
	}
	return "print(" + args
}
