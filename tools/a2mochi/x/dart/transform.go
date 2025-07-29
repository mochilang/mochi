package dart

import (
	"fmt"
	"regexp"
	"strings"

	"mochi/ast"
	"mochi/parser"
)

// Transform converts a parsed Program into a Mochi AST.
func Transform(p *Program) (*ast.Node, error) {
	if p == nil {
		return nil, fmt.Errorf("nil program")
	}

	root := &ast.Node{Kind: "program"}

	// Top-level variables
	for _, stmt := range parseTopLevelVars(p.Src, p.Functions, p.Classes) {
		n, err := parseStmt(stmt)
		if err != nil {
			return nil, err
		}
		root.Children = append(root.Children, n)
	}

	// Classes
	for _, c := range p.Classes {
		root.Children = append(root.Children, classNode(c))
	}

	// Functions
	for _, fn := range p.Functions {
		n, err := funcNode(fn)
		if err != nil {
			return nil, err
		}
		root.Children = append(root.Children, n)
	}

	return root, nil
}

func parseStmt(src string) (*ast.Node, error) {
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	if len(prog.Statements) == 0 {
		return nil, fmt.Errorf("no statement")
	}
	return ast.FromStatement(prog.Statements[0]), nil
}

func classNode(c Class) *ast.Node {
	n := &ast.Node{Kind: "type", Value: c.Name}
	for _, f := range c.Fields {
		fn := &ast.Node{Kind: "field", Value: f.Name}
		if t := toMochiType(f.Type); t != "" && t != "any" {
			fn.Children = append(fn.Children, &ast.Node{Kind: "type", Value: t})
		}
		n.Children = append(n.Children, fn)
	}
	return n
}

func funcNode(fn Function) (*ast.Node, error) {
	n := &ast.Node{Kind: "fun", Value: fn.Name}
	for _, p := range fn.Params {
		pn := &ast.Node{Kind: "param", Value: p.Name}
		if t := toMochiType(p.Type); t != "" && t != "any" {
			pn.Children = append(pn.Children, &ast.Node{Kind: "type", Value: t})
		}
		n.Children = append(n.Children, pn)
	}
	if r := toMochiType(fn.Ret); r != "" {
		n.Children = append(n.Children, &ast.Node{Kind: "type", Value: r})
	}
	for _, line := range fn.Body {
		stmt := convertBodyLine(line)
		st, err := parseStmt(stmt)
		if err != nil {
			return nil, err
		}
		n.Children = append(n.Children, st)
	}
	return n, nil
}

// --- Helpers ---

// typedVarRe matches Dart variable declarations with an optional type.
// It allows "final" or "const" without a following type.
var typedVarRe = regexp.MustCompile(`^(?:final|const)?\s*(?:([A-Za-z_][A-Za-z0-9_<>,\[\]\? ]*)\s+)?([A-Za-z_][A-Za-z0-9_]*)\s*(=.*)?$`)

func parseTopLevelVars(src string, funcs []Function, classes []Class) []string {
	lines := strings.Split(src, "\n")
	skip := make([]bool, len(lines)+1)
	mark := func(start, end int) {
		for i := start; i <= end && i <= len(lines); i++ {
			if i >= 1 {
				skip[i] = true
			}
		}
	}
	for _, f := range funcs {
		mark(f.Start, f.End)
	}
	for _, c := range classes {
		mark(c.Start, c.End)
	}
	var vars []string
	for i, line := range lines {
		ln := i + 1
		if skip[ln] {
			continue
		}
		l := strings.TrimSpace(strings.TrimSuffix(line, ";"))
		if strings.HasPrefix(l, "var ") {
			vars = append(vars, convertQuotes("let "+strings.TrimSpace(l[4:])))
			continue
		}
		if m := typedVarRe.FindStringSubmatch(l); m != nil {
			typ := toMochiType(strings.TrimSpace(m[1]))
			name := m[2]
			val := strings.TrimSpace(strings.TrimPrefix(m[3], "="))
			stmt := "let " + name
			if typ != "" && typ != "any" {
				stmt += ": " + typ
			}
			if val != "" {
				stmt += " = " + val
			}
			vars = append(vars, convertQuotes(stmt))
		}
	}
	return vars
}

var quoteRe = regexp.MustCompile(`'([^']*)'`)

func convertQuotes(s string) string {
	return quoteRe.ReplaceAllStringFunc(s, func(q string) string {
		return "\"" + strings.Trim(q, "'") + "\""
	})
}

var forVarRe = regexp.MustCompile(`^(\s*)for \((?:var|final|const) ([A-Za-z_][A-Za-z0-9_]*) in ([^)]*)\)`)

// forRangeRe matches simple numeric for loops like:
// for (var i = 0; i < n; i++) { ... }
var forRangeRe = regexp.MustCompile(`^(\s*)for \((?:var\s+)?([A-Za-z_][A-Za-z0-9_]*)\s*=\s*([^;]+);\s*[A-Za-z_][A-Za-z0-9_]*\s*<\s*([^;]+);\s*[A-Za-z_][A-Za-z0-9_]*\+\+\)`)

func convertBodyLine(s string) string {
	s = strings.TrimSuffix(s, ";")
	s = forVarRe.ReplaceAllString(s, "${1}for ${2} in ${3}")
	s = forRangeRe.ReplaceAllString(s, "${1}for ${2} in ${3}..${4}")
	if m := typedVarRe.FindStringSubmatch(strings.TrimSpace(s)); m != nil {
		typ := toMochiType(strings.TrimSpace(m[1]))
		name := m[2]
		val := strings.TrimSpace(strings.TrimPrefix(m[3], "="))
		stmt := "var " + name
		if typ != "" && typ != "any" {
			stmt += ": " + typ
		}
		if val != "" {
			stmt += " = " + val
		}
		s = stmt
	}
	if strings.HasPrefix(strings.TrimSpace(s), "let ") {
		s = strings.Replace(s, "let ", "var ", 1)
	}
	s = fixUnaryNeg(s)
	s = convertArrowFunc(s)
	s = convertTernaryPrint(s)
	s = convertTernary(s)
	s = convertSpread(s)
	s = convertLength(s)
	s = convertIsEmpty(s)
	s = convertContains(s)
	s = convertCompareTo(s)
	return convertQuotes(s)
}

var arrowRe = regexp.MustCompile(`\(([^()]*)\)\s*=>`)

func convertArrowFunc(s string) string {
	return arrowRe.ReplaceAllString(s, "fun($1) =>")
}

var unaryNegRe = regexp.MustCompile(`([+\-*/])\s*-([A-Za-z0-9_]+)`) // e.g., "+-2" -> "+ (-2)"

func fixUnaryNeg(s string) string {
	return unaryNegRe.ReplaceAllString(s, "$1 (-$2)")
}

func convertTernaryPrint(s string) string {
	if m := printTernaryRe.FindStringSubmatch(s); m != nil {
		return fmt.Sprintf("print(if %s { %s } else { %s })", strings.TrimSpace(m[1]), strings.TrimSpace(m[2]), strings.TrimSpace(m[3]))
	}
	return s
}

var printTernaryRe = regexp.MustCompile(`^\s*print\(([^?]+)\?\s*([^:]+)\s*:\s*([^)]*)\)$`)

var ternaryRe = regexp.MustCompile(`([^?]+)\?\s*([^:]+)\s*:\s*(.+)`)

func convertTernary(s string) string {
	if ternaryRe.MatchString(s) {
		return ternaryRe.ReplaceAllString(s, "if $1 { $2 } else { $3 }")
	}
	return s
}

var spreadRe = regexp.MustCompile(`\[\.\.\.([A-Za-z_][A-Za-z0-9_]*)\s*,\s*([^\]]+)\]`)

func convertSpread(s string) string {
	if m := spreadPrintRe.FindStringSubmatch(s); m != nil {
		return fmt.Sprintf("print(append(%s, %s))", m[1], m[2])
	}
	return spreadRe.ReplaceAllString(s, "append($1, $2)")
}

var spreadPrintRe = regexp.MustCompile(`^print\("\[" \+ append\(([^,]+),\s*([^\)]+)\)\.join\("[, ]*"\) \+ "\]"\)$`)

var lengthRe = regexp.MustCompile(`([A-Za-z0-9_\]\)]+)\.length`)

func convertLength(s string) string {
	return lengthRe.ReplaceAllString(s, "len($1)")
}

var isEmptyNotRe = regexp.MustCompile(`!\s*([A-Za-z0-9_\]\)]+)\.isEmpty`)
var isEmptyRe = regexp.MustCompile(`([A-Za-z0-9_\]\)]+)\.isEmpty`)

func convertIsEmpty(s string) string {
	s = isEmptyNotRe.ReplaceAllString(s, "len($1) != 0")
	return isEmptyRe.ReplaceAllString(s, "len($1) == 0")
}

var containsNotRe = regexp.MustCompile(`!\s*([A-Za-z0-9_]+)\.contains(?:Key)?\(([^)]+)\)`)
var containsRe = regexp.MustCompile(`([A-Za-z0-9_]+)\.contains(?:Key)?\(([^)]+)\)`)

func convertContains(s string) string {
	s = containsNotRe.ReplaceAllString(s, "!($2 in $1)")
	return containsRe.ReplaceAllString(s, "$2 in $1")
}

var cmpLTRe = regexp.MustCompile(`([\"A-Za-z0-9_]+)\.compareTo\(([\"A-Za-z0-9_]+)\)\s*<\s*0`)
var cmpLERe = regexp.MustCompile(`([\"A-Za-z0-9_]+)\.compareTo\(([\"A-Za-z0-9_]+)\)\s*<=\s*0`)
var cmpGTRe = regexp.MustCompile(`([\"A-Za-z0-9_]+)\.compareTo\(([\"A-Za-z0-9_]+)\)\s*>\s*0`)
var cmpGERe = regexp.MustCompile(`([\"A-Za-z0-9_]+)\.compareTo\(([\"A-Za-z0-9_]+)\)\s*>=\s*0`)

func convertCompareTo(s string) string {
	s = cmpLERe.ReplaceAllString(s, "$1 <= $2")
	s = cmpLTRe.ReplaceAllString(s, "$1 < $2")
	s = cmpGERe.ReplaceAllString(s, "$1 >= $2")
	s = cmpGTRe.ReplaceAllString(s, "$1 > $2")
	return s
}

func splitArgs(s string) []string {
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

func toMochiType(t string) string {
	t = strings.TrimSpace(t)
	if strings.HasSuffix(t, "?") {
		t = strings.TrimSuffix(t, "?")
	}
	switch t {
	case "", "dynamic", "Object":
		return "any"
	case "int":
		return "int"
	case "double", "num":
		return "float"
	case "bool":
		return "bool"
	case "String":
		return "string"
	case "void":
		return ""
	}
	if strings.HasSuffix(t, ">") {
		if open := strings.Index(t, "<"); open != -1 {
			outer := strings.TrimSpace(t[:open])
			inner := strings.TrimSuffix(t[open+1:], ">")
			args := splitArgs(inner)
			switch outer {
			case "List", "Iterable", "Set":
				a := "any"
				if len(args) > 0 {
					if at := toMochiType(args[0]); at != "" {
						a = at
					}
				}
				return "list<" + a + ">"
			case "Map":
				if len(args) == 2 {
					k := toMochiType(args[0])
					if k == "" {
						k = "any"
					}
					v := toMochiType(args[1])
					if v == "" {
						v = "any"
					}
					return "map<" + k + ", " + v + ">"
				}
			case "Future":
				if len(args) == 1 {
					return toMochiType(args[0])
				}
			}
		}
	}
	return t
}
