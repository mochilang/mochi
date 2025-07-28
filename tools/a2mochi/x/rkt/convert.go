//go:build slow

package rkt

import (
	"fmt"
	"os"
	"regexp"
	"strings"

	"mochi/ast"
	"mochi/parser"
	"mochi/transpiler/meta"
)

type Item struct {
	Kind    string
	Name    string
	Params  []string
	Fields  []string
	Options []string
	Body    string
	Value   string
}

// Parse parses Racket source into a slice of Items. Only a small subset of
// syntax is recognised.
func Parse(src string) ([]Item, error) {
	items := parse(src)
	out := make([]Item, len(items))
	for i, it := range items {
		out[i] = Item(it)
	}
	return out, nil
}

// ConvertSource converts parsed Items into Mochi source code.
func ConvertSource(items []Item, src string) string {
	internal := make([]item, len(items))
	for i, it := range items {
		internal[i] = item(it)
	}
	var b strings.Builder
	b.Write(meta.Header("//"))
	if strings.TrimSpace(src) != "" {
		b.WriteString("/*\n")
		b.WriteString(src)
		if !strings.HasSuffix(src, "\n") {
			b.WriteByte('\n')
		}
		b.WriteString("*/\n")
	}
	writeItems(&b, internal)
	parseToplevel(&b, src)
	return b.String()
}

// Convert parses Racket source and returns a Mochi AST node.
func Convert(src string) (*ast.Node, error) {
	items := parse(src)
	var b strings.Builder
	writeItems(&b, items)
	parseToplevel(&b, src)
	code := strings.TrimSpace(b.String())
	if code == "" {
		return nil, fmt.Errorf("no convertible symbols found")
	}
	prog, err := parser.ParseString(code)
	if err != nil {
		return nil, err
	}
	return ast.FromProgram(prog), nil
}

// ConvertFile reads the Racket file at path and converts it to a Mochi AST.
func ConvertFile(path string) (*ast.Node, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Convert(string(data))
}

// ----- minimal parser -----

type item Item

func tokenize(src string) []string {
	var toks []string
	var buf strings.Builder
	inStr := false
	escape := false
	for _, r := range src {
		switch {
		case inStr:
			buf.WriteRune(r)
			if escape {
				escape = false
			} else if r == '\\' {
				escape = true
			} else if r == '"' {
				toks = append(toks, buf.String())
				buf.Reset()
				inStr = false
			}
		case r == '"':
			if buf.Len() > 0 {
				toks = append(toks, buf.String())
				buf.Reset()
			}
			buf.WriteRune(r)
			inStr = true
		case r == '(' || r == ')':
			if buf.Len() > 0 {
				toks = append(toks, buf.String())
				buf.Reset()
			}
			toks = append(toks, string(r))
		case strings.ContainsRune(" \t\n\r", r):
			if buf.Len() > 0 {
				toks = append(toks, buf.String())
				buf.Reset()
			}
		default:
			buf.WriteRune(r)
		}
	}
	if buf.Len() > 0 {
		toks = append(toks, buf.String())
	}
	return toks
}

func parseTokens(toks []string) []item {
	var items []item
	stack := 0
	i := 0
	for i < len(toks) {
		if toks[i] == "(" && i+1 < len(toks) && toks[i+1] == "struct" && stack == 0 {
			i += 2
			if i >= len(toks) {
				break
			}
			name := toks[i]
			i++
			var fields []string
			var opts []string
			if i < len(toks) && toks[i] == "(" {
				i++
				for i < len(toks) && toks[i] != ")" {
					fields = append(fields, toks[i])
					i++
				}
				if i < len(toks) {
					i++
				}
			}
			for i < len(toks) && toks[i] != ")" {
				opts = append(opts, toks[i])
				i++
			}
			if i < len(toks) {
				i++
			}
			items = append(items, item{Kind: "struct", Name: name, Fields: fields, Options: opts})
			continue
		}
		if toks[i] == "(" && i+1 < len(toks) && toks[i+1] == "displayln" && stack == 0 {
			i += 2
			start := i
			depth := 0
			for i < len(toks) {
				if toks[i] == "(" {
					depth++
				} else if toks[i] == ")" {
					if depth == 0 {
						break
					}
					depth--
				}
				i++
			}
			expr := strings.Join(toks[start:i], " ")
			items = append(items, item{Kind: "print", Value: expr})
			i++
			continue
		}
		if toks[i] == "(" && i+1 < len(toks) && toks[i+1] == "define" && stack == 0 {
			i += 2
			if i < len(toks) && toks[i] == "(" {
				i++
				if i >= len(toks) {
					break
				}
				name := toks[i]
				i++
				var params []string
				for i < len(toks) && toks[i] != ")" {
					params = append(params, toks[i])
					i++
				}
				i++
				start := i
				depth := 1
				for i < len(toks) && depth > 0 {
					if toks[i] == "(" {
						depth++
					} else if toks[i] == ")" {
						depth--
					}
					i++
				}
				body := strings.Join(toks[start:i-1], " ")
				items = append(items, item{Kind: "func", Name: name, Params: params, Body: body})
			} else {
				if i >= len(toks) {
					break
				}
				name := toks[i]
				i++
				start := i
				depth := 0
				for i < len(toks) {
					if toks[i] == "(" {
						depth++
					} else if toks[i] == ")" {
						if depth == 0 {
							break
						}
						depth--
					}
					i++
				}
				value := strings.Join(toks[start:i], " ")
				items = append(items, item{Kind: "var", Name: name, Value: value})
				i++
			}
		} else {
			if toks[i] == "(" {
				stack++
			} else if toks[i] == ")" {
				if stack > 0 {
					stack--
				}
			}
			i++
		}
	}
	return items
}

func parse(src string) []item {
	return parseTokens(tokenize(src))
}

// ---- helpers for conversion ----

func parseToplevel(out *strings.Builder, src string) {
	forIdx := strings.Index(src, "(for")
	for forIdx != -1 {
		close := findMatch(src, forIdx, '(', ')')
		if close == len(src) {
			break
		}
		snippet := src[forIdx : close+1]
		headerRe := regexp.MustCompile(`(?s)^\(for\s*\(\s*\[([A-Za-z0-9_-]+)\s+([^\]]+)\]\s*\)\s*(.+)\)$`)
		if hm := headerRe.FindStringSubmatch(snippet); len(hm) == 4 {
			varName := strings.TrimSpace(hm[1])
			collection := strings.TrimSpace(hm[2])
			body := strings.TrimSpace(hm[3])
			bodyPrint := regexp.MustCompile(`^\(displayln\s+(.+)\)$`)
			if bm := bodyPrint.FindStringSubmatch(body); len(bm) == 2 {
				expr := strings.TrimSpace(bm[1])
				fmt.Fprintf(out, "for %s in %s {\n  print(%s)\n}\n", varName, convertExpr(collection), convertExpr(expr))
			}
		}
		next := strings.Index(src[close+1:], "(for")
		if next == -1 {
			break
		}
		forIdx = close + 1 + next
	}
}

func parseValue(expr string) string {
	expr = strings.TrimSpace(expr)
	if strings.HasPrefix(expr, "(") && strings.HasSuffix(expr, ")") {
		inner := strings.TrimSpace(expr[1 : len(expr)-1])
		expr = "(" + inner + ")"
	}
	if strings.HasPrefix(expr, "\"") {
		return expr
	}
	if strings.HasPrefix(expr, "'") {
		expr = strings.TrimPrefix(expr, "'")
	}
	if regexp.MustCompile(`^[0-9-]+$`).MatchString(expr) {
		return expr
	}
	if (strings.HasPrefix(expr, "(list ") || expr == "(list)") && strings.HasSuffix(expr, ")") {
		inner := strings.TrimSpace(expr[len("(list") : len(expr)-1])
		if inner == "" {
			return "[]"
		}
		itemRe := regexp.MustCompile(`\(hash[^\)]*\)`)
		items := itemRe.FindAllString(inner, -1)
		var out []string
		if len(items) > 0 {
			for _, it := range items {
				out = append(out, parseHash(it))
			}
		} else {
			parts := strings.Fields(inner)
			for _, p := range parts {
				out = append(out, convertExpr(p))
			}
		}
		if len(out) > 0 {
			return "[" + strings.Join(out, ", ") + "]"
		}
	}
	if strings.HasPrefix(expr, "(hash") && strings.HasSuffix(expr, ")") {
		return parseHash(expr)
	}
	if out := convertExpr(expr); out != expr {
		return out
	}
	return expr
}

func parseHash(expr string) string {
	inner := strings.TrimSpace(expr[len("(hash") : len(expr)-1])
	fields := strings.Fields(inner)
	var pairs []string
	for i := 0; i+1 < len(fields); i += 2 {
		key := strings.Trim(fields[i], `"`)
		val := convertExpr(fields[i+1])
		pairs = append(pairs, fmt.Sprintf("%s: %s", key, val))
	}
	return "{" + strings.Join(pairs, ", ") + "}"
}

func convertExpr(expr string) string {
	expr = strings.TrimSpace(expr)
	if strings.HasPrefix(expr, "(hash-ref") {
		parts := strings.Fields(expr)
		if len(parts) == 3 {
			return fmt.Sprintf("%s[%s]", parts[1], strings.Trim(parts[2], `"`))
		}
	} else if strings.HasPrefix(expr, "(in-range") {
		inner := strings.TrimSpace(expr[len("(in-range") : len(expr)-1])
		args := strings.Fields(inner)
		if len(args) == 2 {
			return fmt.Sprintf("%s..%s", args[0], args[1])
		}
		return fmt.Sprintf("range(%s)", strings.Join(args, ", "))
	} else if trimmed := strings.TrimSpace(expr[1:]); strings.HasPrefix(trimmed, "list ") || strings.HasPrefix(trimmed, "list)") {
		return parseValue(expr)
	} else if strings.HasPrefix(expr, "(") && strings.HasSuffix(expr, ")") {
		inner := strings.TrimSpace(expr[1 : len(expr)-1])
		var parts []string
		depth := 0
		start := 0
		inStr := false
		escape := false
		for i, r := range inner {
			if inStr {
				if escape {
					escape = false
				} else if r == '\\' {
					escape = true
				} else if r == '"' {
					inStr = false
				}
				continue
			}
			switch r {
			case '"':
				inStr = true
			case '(':
				depth++
			case ')':
				depth--
			case ' ':
				if depth == 0 {
					if start < i {
						parts = append(parts, inner[start:i])
					}
					start = i + 1
					continue
				}
			}
		}
		if start < len(inner) {
			parts = append(parts, inner[start:])
		}
		if len(parts) == 2 && parts[0] == "-" {
			return "-" + convertExpr(parts[1])
		}
		if len(parts) == 3 && (parts[0] == "+" || parts[0] == "-" || parts[0] == "*" || parts[0] == "/") {
			a := convertExpr(parts[1])
			b := convertExpr(parts[2])
			if strings.HasPrefix(b, "-") {
				b = "(" + b + ")"
			}
			return fmt.Sprintf("%s %s %s", a, parts[0], b)
		}
		if len(parts) == 3 && (parts[0] == "=" || parts[0] == "<" || parts[0] == ">" || parts[0] == "<=" || parts[0] == ">=") {
			a := convertExpr(parts[1])
			b := convertExpr(parts[2])
			op := parts[0]
			if op == "=" {
				op = "=="
			}
			return fmt.Sprintf("%s %s %s", a, op, b)
		}
		if len(parts) == 3 && (parts[0] == "list-ref" || parts[0] == "string-ref") {
			target := convertExpr(parts[1])
			idx := convertExpr(parts[2])
			return fmt.Sprintf("%s[%s]", target, idx)
		}
		if len(parts) >= 2 && parts[0] == "string-append" {
			var args []string
			for _, p := range parts[1:] {
				args = append(args, convertExpr(p))
			}
			return strings.Join(args, " + ")
		}
		if len(parts) >= 1 {
			var args []string
			for _, p := range parts[1:] {
				args = append(args, convertExpr(p))
			}
			return fmt.Sprintf("%s(%s)", parts[0], strings.Join(args, ", "))
		}
	}
	return expr
}

func findMatch(s string, openIdx int, open, close rune) int {
	depth := 0
	for i, r := range s[openIdx:] {
		if r == open {
			depth++
		} else if r == close {
			depth--
			if depth == 0 {
				return openIdx + i
			}
		}
	}
	return len(s)
}

func writeItems(out *strings.Builder, items []item) {
	for _, it := range items {
		switch it.Kind {
		case "func":
			out.WriteString("fun ")
			out.WriteString(it.Name)
			out.WriteByte('(')
			if len(it.Params) > 0 {
				for i, p := range it.Params {
					if i > 0 {
						out.WriteString(", ")
					}
					out.WriteString(p)
					out.WriteString(": int")
				}
			}
			out.WriteByte(')')
			out.WriteString(": int")
			body := bodyFromSnippet(it.Body)
			if len(body) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, st := range body {
					out.WriteString("  ")
					out.WriteString(st)
					out.WriteByte('\n')
				}
				out.WriteString("}\n")
			}
		case "var":
			out.WriteString("let ")
			out.WriteString(it.Name)
			if val := parseValue(it.Value); val != "" {
				out.WriteString(" = ")
				out.WriteString(val)
			}
			out.WriteByte('\n')
		case "struct":
			// struct declarations in generated Racket code do not carry
			// type information useful for Mochi programs. They are ignored
			// to keep the output parsable.
			continue
		case "print":
			out.WriteString("print(")
			out.WriteString(convertExpr(it.Value))
			out.WriteString(")\n")
		}
	}
}

func bodyFromSnippet(snippet string) []string {
	snippet = strings.TrimSpace(snippet)
	var out []string
	lineRe := regexp.MustCompile(`\([^()]+\)`)
	matches := lineRe.FindAllString(snippet, -1)
	if len(matches) == 0 || len(matches[0]) != len(snippet) {
		if snippet != "" {
			out = append(out, "return "+convertExpr(snippet))
		}
		return out
	}
	for _, m := range matches {
		stmt := strings.TrimSpace(m)
		if strings.HasPrefix(stmt, "(displayln") {
			expr := strings.TrimSpace(stmt[len("(displayln") : len(stmt)-1])
			out = append(out, "print("+convertExpr(expr)+")")
		} else if strings.HasPrefix(stmt, "(return") {
			expr := strings.TrimSpace(stmt[len("(return") : len(stmt)-1])
			if expr != "" && expr != "(void)" {
				out = append(out, "return "+convertExpr(expr))
			} else {
				out = append(out, "return")
			}
		} else {
			out = append(out, "return "+convertExpr(stmt))
		}
	}
	return out
}
