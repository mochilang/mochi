package any2mochi

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"regexp"
	"strings"
	"time"
)

// ConvertOcaml converts OCaml source code to Mochi. It relies on the
// "obolcaml" CLI which parses the input and returns a JSON encoded AST.
// The converter only understands a very small subset of nodes.
func ConvertOcaml(src string) ([]byte, error) {
	ls := Servers["ocaml"]
	if err := EnsureServer(ls.Command); err != nil {
		return nil, err
	}
	decls, err := runOcamlParse(ls.Command, ls.Args, src)
	if err != nil {
		return nil, err
	}
	out := convertOcamlDecls(decls)
	if len(out) == 0 {
		// fall back to the tiny regex based parser for trivial snippets
		out = parseOcamlLines(strings.Split(src, "\n"))
	}
	if len(out) == 0 {
		return nil, fmt.Errorf("no convertible statements found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(strings.Join(out, "\n")), nil
}

// ConvertOcamlFile reads the OCaml file and converts it to Mochi.
func ConvertOcamlFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertOcaml(string(data))
}

// ocamlDecl represents a simplified OCaml declaration returned by the
// `obolcaml parse --json` command.
type ocamlDecl struct {
	Kind   string   `json:"kind"`
	Name   string   `json:"name"`
	Type   string   `json:"type,omitempty"`
	Params []string `json:"params,omitempty"`
}

func runOcamlParse(cmd string, args []string, src string) ([]ocamlDecl, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	c := exec.CommandContext(ctx, cmd, args...)
	c.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	c.Stdout = &out
	if err := c.Run(); err != nil {
		return nil, err
	}
	var decls []ocamlDecl
	if err := json.Unmarshal(out.Bytes(), &decls); err != nil {
		return nil, err
	}
	return decls, nil
}

func convertOcamlDecls(decls []ocamlDecl) []string {
	var out []string
	for _, d := range decls {
		switch d.Kind {
		case "fun", "function":
			out = append(out, fmt.Sprintf("fun %s(%s) {}", d.Name, strings.Join(d.Params, ", ")))
		case "let", "var":
			line := "let " + d.Name
			if d.Type != "" {
				line += ": " + d.Type
			}
			out = append(out, line)
		}
	}
	return out
}

func parseOcamlLines(lines []string) []string {
	var out []string
	for i := 0; i < len(lines); i++ {
		t := strings.TrimSpace(lines[i])
		if t == "" {
			continue
		}
		if strings.HasPrefix(t, "while ") {
			cond := strings.TrimSpace(strings.TrimSuffix(strings.TrimPrefix(t, "while"), "do"))
			var body []string
			for i = i + 1; i < len(lines); i++ {
				s := strings.TrimSpace(lines[i])
				if s == "done" || s == "done;;" {
					break
				}
				if s != "" {
					body = append(body, "  "+convertOcamlLine(s))
				}
			}
			out = append(out, "while "+convertOcamlExpr(cond)+" {")
			out = append(out, body...)
			out = append(out, "}")
			continue
		}
		conv := convertOcamlLine(t)
		if conv != "" {
			out = append(out, conv)
		}
	}
	return out
}

func convertOcamlLine(line string) string {
	line = strings.TrimSpace(line)
	line = strings.TrimSuffix(line, ";;")
	if strings.HasPrefix(line, "let ") {
		return convertOcamlLet(strings.TrimPrefix(line, "let "))
	}
	if strings.Contains(line, ":=") {
		parts := strings.SplitN(line, ":=", 2)
		return strings.TrimSpace(parts[0]) + " = " + convertOcamlExpr(parts[1])
	}
	if strings.HasPrefix(line, "print_endline") {
		return convertOcamlExpr(line)
	}
	return convertOcamlExpr(line)
}

var reFunExpr = regexp.MustCompile(`^fun\s+(.*?)\s*->\s*(.*)$`)

func convertOcamlLet(rest string) string {
	rest = strings.TrimSpace(rest)
	if strings.HasPrefix(rest, "rec ") {
		rest = strings.TrimSpace(rest[4:])
	}
	eq := strings.Index(rest, "=")
	if eq == -1 {
		return ""
	}
	left := strings.TrimSpace(rest[:eq])
	right := strings.TrimSpace(rest[eq+1:])

	if m := reFunExpr.FindStringSubmatch(right); m != nil {
		params := parseOcamlParams(m[1])
		body := convertOcamlExpr(m[2])
		return fmt.Sprintf("var %s = fun(%s) => %s", left, strings.Join(params, ", "), body)
	}

	parts := strings.Fields(left)
	name := parts[0]
	if len(parts) > 1 {
		params := parseOcamlParams(strings.Join(parts[1:], " "))
		body := convertOcamlExpr(right)
		return fmt.Sprintf("fun %s(%s) => %s", name, strings.Join(params, ", "), body)
	}
	if strings.HasPrefix(right, "ref ") {
		right = strings.TrimSpace(strings.TrimPrefix(right, "ref "))
		return fmt.Sprintf("var %s = %s", name, convertOcamlExpr(right))
	}
	return fmt.Sprintf("let %s = %s", name, convertOcamlExpr(right))
}

func parseOcamlParams(s string) []string {
	var params []string
	for _, f := range strings.Fields(s) {
		f = strings.TrimPrefix(f, "(")
		if i := strings.Index(f, ")"); i != -1 {
			f = f[:i]
		}
		if i := strings.Index(f, ":"); i != -1 {
			f = f[:i]
		}
		f = strings.TrimSpace(f)
		if f != "" {
			params = append(params, f)
		}
	}
	return params
}

func convertOcamlExpr(e string) string {
	e = strings.TrimSpace(e)
	e = strings.ReplaceAll(e, "print_endline", "print")
	e = strings.ReplaceAll(e, "string_of_int", "str")
	e = strings.ReplaceAll(e, "string_of_float", "str")
	e = strings.ReplaceAll(e, "string_of_bool", "str")
	e = strings.ReplaceAll(e, ":=", "=")
	e = strings.ReplaceAll(e, "!", "")
	e = strings.ReplaceAll(e, ";", ",")
	return e
}
