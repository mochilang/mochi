//go:build slow

package ocaml

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"

	"mochi/ast"
	"mochi/parser"
)

// Transform converts the parsed OCaml program into a Mochi AST. The
// conversion is intentionally tiny and handles just the small subset of
// constructs present in the example sources used by the tests.
func Transform(p *Program) (*ast.Node, error) {
	if p == nil {
		return nil, fmt.Errorf("nil program")
	}
	code := buildMochi(p)
	prog, err := parser.ParseString(code)
	if err != nil {
		return nil, err
	}
	return ast.FromProgram(prog), nil
}

func buildMochi(p *Program) string {
	var out strings.Builder
	for _, pr := range p.Prints {
		for _, line := range convertExpr(pr.Expr) {
			out.WriteString(line)
			out.WriteByte('\n')
		}
	}
	for _, v := range p.Vars {
		expr := simplify(v.Expr)
		if v.Mutable {
			fmt.Fprintf(&out, "var %s = %s\n", v.Name, expr)
		} else {
			fmt.Fprintf(&out, "let %s = %s\n", v.Name, expr)
		}
	}
	return out.String()
}

// convertExpr converts a single OCaml expression that may contain nested let
// bindings and multiple statements separated by semicolons. The returned slice
// contains Mochi statements.
func convertExpr(expr string) []string {
	expr = strings.ReplaceAll(expr, "\r", "")
	expr = strings.TrimSpace(expr)
	var lines []string

	// handle leading let-bindings: let x = ... in ...
	reLet := regexp.MustCompile(`(?s)^let\s+([A-Za-z0-9_']+)\s*=\s*(.*?)\s+in\s*(.*)$`)
	for {
		m := reLet.FindStringSubmatch(expr)
		if m == nil {
			break
		}
		name := m[1]
		val := simplify(m[2])
		lines = append(lines, fmt.Sprintf("let %s = %s", name, val))
		expr = strings.TrimSpace(m[3])
	}

	parts := strings.Split(expr, ";")
	for _, p := range parts {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		if strings.Contains(p, ":=") {
			fs := strings.SplitN(p, ":=", 2)
			left := strings.TrimSpace(fs[0])
			right := simplify(strings.TrimSpace(fs[1]))
			lines = append(lines, fmt.Sprintf("%s = %s", left, right))
			continue
		}
		if strings.HasPrefix(p, "print_endline") {
			arg := unwrapPrint(p)
			lines = append(lines, fmt.Sprintf("print(%s)", simplify(arg)))
			continue
		}
		lines = append(lines, simplify(p))
	}
	return lines
}

func unwrapPrint(s string) string {
	s = strings.TrimSpace(s)
	if strings.HasPrefix(s, "print_endline") {
		s = strings.TrimPrefix(s, "print_endline")
		s = strings.TrimSpace(s)
		if strings.HasPrefix(s, "(") && strings.HasSuffix(s, ")") {
			s = strings.TrimSuffix(strings.TrimPrefix(s, "("), ")")
		}
	}
	s = strings.TrimSpace(s)
	if strings.HasPrefix(s, "String.concat \" \"") {
		s = strings.TrimPrefix(s, "String.concat \" \"")
		s = strings.TrimSpace(s)
		if strings.HasPrefix(s, "(") && strings.HasSuffix(s, ")") {
			s = strings.TrimSuffix(strings.TrimPrefix(s, "("), ")")
		}
		if strings.HasPrefix(s, "List.filter (fun s -> s <> \"\") [") && strings.HasSuffix(s, "]") {
			s = strings.TrimSuffix(strings.TrimPrefix(s, "List.filter (fun s -> s <> \"\") ["), "]")
		} else if strings.HasPrefix(s, "[") && strings.HasSuffix(s, "]") {
			s = strings.TrimSuffix(strings.TrimPrefix(s, "["), "]")
		}
	}
	return strings.TrimSpace(s)
}

func simplify(e string) string {
	e = strings.TrimSpace(e)
	e = strings.ReplaceAll(e, "string_of_int", "")
	e = strings.ReplaceAll(e, "string_of_float", "")
	e = strings.ReplaceAll(e, "string_of_bool", "")
	e = strings.ReplaceAll(e, "not ", "!")
	e = strings.ReplaceAll(e, "ref ", "")

	// List and string length
	e = strings.ReplaceAll(e, "List.length", "len")
	e = strings.ReplaceAll(e, "String.length", "len")

	// list syntax
	e = strings.ReplaceAll(e, ";", ",")

	re := regexp.MustCompile(`String\.get\s+([A-Za-z0-9_]+)\s+(\d+)`)
	e = re.ReplaceAllString(e, `$1[$2]`)

	re = regexp.MustCompile(`String\.make\s+1\s+\(([^\)]+)\)`)
	e = re.ReplaceAllString(e, `$1`)

	re = regexp.MustCompile(`String\.sub\s+("[^"]+"|[A-Za-z0-9_]+)\s+(\d+)\s+\((\d+)\s*-\s*(\d+)\)`)
	if m := re.FindStringSubmatch(e); m != nil {
		start, _ := strconv.Atoi(m[2])
		end, _ := strconv.Atoi(m[3])
		e = re.ReplaceAllString(e, fmt.Sprintf("substring(%s, %d, %d)", m[1], start, end))
	} else {
		re = regexp.MustCompile(`String\.sub\s+("[^"]+"|[A-Za-z0-9_]+)\s+(\d+)\s+(\d+)`)
		if m := re.FindStringSubmatch(e); m != nil {
			start, _ := strconv.Atoi(m[2])
			ln, _ := strconv.Atoi(m[3])
			end := start + ln
			e = re.ReplaceAllString(e, fmt.Sprintf("substring(%s, %d, %d)", m[1], start, end))
		}
	}

	e = strings.TrimSpace(e)
	return e
}
