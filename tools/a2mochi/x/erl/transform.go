//go:build slow

package erl

import (
	"fmt"
	"regexp"
	"strings"

	"mochi/ast"
	"mochi/parser"
)

var assignRe = regexp.MustCompile(`^([A-Za-z_][A-Za-z0-9_]*)\s*=\s*(.+)$`)
var appendRe = regexp.MustCompile(`lists:append\(([^,]+),\s*\[([^\]]+)\]\)`)
var mapsGetRe = regexp.MustCompile(`maps:get\(([^,]+),\s*([^\)]+)\)`)
var mapsPutRe = regexp.MustCompile(`maps:put\(([^,]+),\s*([^,]+),\s*([^\)]+)\)`)
var mapsIsKeyRe = regexp.MustCompile(`maps:is_key\(([^,]+),\s*([^\)]+)\)`)
var stringStrNotZeroRe = regexp.MustCompile(`string:str\(([^,]+),\s*([^\)]+)\)\s*(=/=|/=)\s*0`)
var stringStrZeroRe = regexp.MustCompile(`string:str\(([^,]+),\s*([^\)]+)\)\s*(=|=:=)\s*0`)
var substrRe = regexp.MustCompile(`string:substr\(([^,]+),\s*([^,]+?)\s*\+\s*1,\s*([^\)]+)\)`)
var plusNegRe = regexp.MustCompile(`\+\s*-([0-9]+)`)
var listsNthRe = regexp.MustCompile(`lists:nth\(([^,]+?)\s*\+\s*1,\s*([^\)]+)\)`)
var listsMemberRe = regexp.MustCompile(`lists:member\(([^,]+),\s*([^\)]+)\)`)
var assignMapPutRe = regexp.MustCompile(`^\s*[A-Za-z_][A-Za-z0-9_]*\s*=\s*maps:put\(([^,]+),\s*([^,]+),\s*([^\)]+)\)`)
var notMemberRe = regexp.MustCompile(`not\s+lists:member\(([^,]+),\s*([^\)]+)\)`)

func node(kind string, value any, children ...*ast.Node) *ast.Node {
	return &ast.Node{Kind: kind, Value: value, Children: children}
}

func fromParsedProgram(p *parser.Program) *ast.Node {
	root := node("program", p.Package)
	for _, s := range p.Statements {
		root.Children = append(root.Children, ast.FromStatement(s))
	}
	return root
}

// Transform converts a Program into a Mochi AST node.
func Transform(p *Program) (*ast.Node, error) {
	if p == nil {
		return nil, fmt.Errorf("nil program")
	}
	src, err := formatProgram(p)
	if err != nil {
		return nil, err
	}
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	return fromParsedProgram(prog), nil
}

func formatProgram(p *Program) (string, error) {
	if p == nil {
		return "", fmt.Errorf("nil program")
	}
	var parts []string
	if p.Module != "" {
		parts = append(parts, "package "+p.Module, "")
	}
	for _, r := range p.Records {
		parts = append(parts, "// line "+fmt.Sprint(r.Line))
		if r.EndLine > 0 && r.EndLine != r.Line {
			parts[len(parts)-1] += "-" + fmt.Sprint(r.EndLine)
		}
		parts = append(parts, "type "+strings.Title(r.Name)+" {")
		for _, f := range r.Fields {
			parts = append(parts, "  "+f+": any")
		}
		parts = append(parts, "}")
	}
	hasMain := false
	for _, f := range p.Functions {
		if f.Name == "main" {
			hasMain = true
		}
		if strings.HasPrefix(f.Name, "mochi_") && f.Name != "main" {
			continue
		}
		header := "// line " + fmt.Sprint(f.Line)
		if f.EndLine > 0 && f.EndLine != f.Line {
			header += "-" + fmt.Sprint(f.EndLine)
		}
		if f.Exported {
			header += " (exported)"
		}
		parts = append(parts, header)
		funLine := "fun "
		if f.Name == "" {
			funLine += "fun"
		} else {
			funLine += f.Name
		}
		funLine += "("
		params := f.Params
		if f.Name == "main" && len(params) == 1 && params[0] == "_" {
			params = nil
		}
		boolReturn := false
		if len(f.Body) > 0 {
			last := strings.TrimSpace(f.Body[len(f.Body)-1])
			if last == "true" || last == "false" || strings.HasPrefix(last, "return ") {
				boolReturn = true
			}
		}
		for i, p := range params {
			if i > 0 {
				funLine += ", "
			}
			funLine += p
			if p != "_" {
				funLine += ": any"
			}
		}
		funLine += ")"
		if boolReturn {
			funLine += ": bool"
		} else if len(params) > 0 {
			funLine += ": any"
		}
		if len(f.Body) == 0 {
			parts = append(parts, funLine+" {}")
		} else {
			parts = append(parts, funLine+" {")
			for _, line := range f.Body {
				ln := strings.ReplaceAll(line, "\n", " ")
				ln = rewriteLine(ln, p.Records)
				parts = append(parts, "  "+strings.TrimSpace(ln))
			}
			parts = append(parts, "}")
		}
	}
	if hasMain {
		parts = append(parts, "main()")
	}
	if len(parts) == 0 {
		return "", fmt.Errorf("no convertible symbols found")
	}
	return strings.Join(parts, "\n") + "\n", nil
}

func rewriteLine(ln string, recs []Record) string {
	ln = strings.TrimSuffix(strings.TrimSpace(ln), ",")
	if m := assignMapPutRe.FindStringSubmatch(ln); m != nil {
		return strings.TrimSpace(m[3]) + "[" + strings.TrimSpace(m[1]) + "] = " + strings.TrimSpace(m[2])
	}
	if listsNthRe.MatchString(ln) {
		ln = listsNthRe.ReplaceAllStringFunc(ln, func(s string) string {
			m := listsNthRe.FindStringSubmatch(s)
			return strings.TrimSpace(m[2]) + "[" + strings.TrimSpace(m[1]) + "]"
		})
	}
	if notMemberRe.MatchString(ln) {
		ln = notMemberRe.ReplaceAllString(ln, "!($1 in $2)")
	} else if listsMemberRe.MatchString(ln) {
		ln = listsMemberRe.ReplaceAllString(ln, "$1 in $2")
	}
	if strings.HasPrefix(ln, "io:format(") {
		ln = strings.TrimPrefix(ln, "io:format(")
		ln = rewritePrint(ln)
	} else if strings.HasPrefix(ln, "io:fwrite(") {
		ln = strings.TrimPrefix(ln, "io:fwrite(")
		ln = rewritePrint(ln)
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
	if strings.Contains(ln, "list_to_integer(") {
		ln = strings.ReplaceAll(ln, "list_to_integer(", "int(")
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
	if strings.Contains(ln, "++") {
		ln = strings.ReplaceAll(ln, "++", "+")
	}
	if strings.Contains(ln, "+ -") {
		ln = plusNegRe.ReplaceAllString(ln, "+ (-$1)")
	}
	if stringStrNotZeroRe.MatchString(ln) {
		ln = stringStrNotZeroRe.ReplaceAllString(ln, "$1.contains($2)")
	}
	if stringStrZeroRe.MatchString(ln) {
		ln = stringStrZeroRe.ReplaceAllString(ln, "!$1.contains($2)")
	}
	if substrRe.MatchString(ln) {
		ln = substrRe.ReplaceAllStringFunc(ln, func(s string) string {
			m := substrRe.FindStringSubmatch(s)
			start := strings.TrimSpace(m[2])
			length := strings.TrimSpace(m[3])
			target := strings.TrimSpace(m[1])
			if length == "1" {
				return target + "[" + start + "]"
			}
			return "substring(" + target + ", " + start + ", " + start + " + " + length + ")"
		})
	}
	if strings.Contains(ln, "andalso") {
		ln = strings.ReplaceAll(ln, "andalso", "&&")
	}
	if strings.Contains(ln, "orelse") {
		ln = strings.ReplaceAll(ln, "orelse", "||")
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
	if ln == "true" || ln == "false" {
		ln = "return " + ln
	}
	return ln
}

var printfRe = regexp.MustCompile(`^"~[sp]~n",\s*\[(.+)\]\)$`)

func rewritePrint(args string) string {
	trimmed := strings.TrimSpace(args)
	if m := printfRe.FindStringSubmatch(trimmed); m != nil {
		return "print(" + m[1] + ")"
	}
	return "print(" + trimmed
}
