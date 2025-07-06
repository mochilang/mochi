package any2mochi

import (
	"bytes"
	_ "embed"
	"encoding/json"
	"io"
	"os"
	"os/exec"
	"regexp"
	"strings"
)

//go:embed pl2ast.pl
var pl2astScript []byte

// PrologFunc represents a parsed Prolog predicate.
type PrologFunc struct {
	Name   string   // predicate name
	Params []string // parameter names
	Body   string   // body of predicate without trailing period
}

var prologDefRE = regexp.MustCompile(`(?ms)^\s*(\w+)(?:\(([^)]*)\))?\s*:-\s*(.*?)\.`)

// ParseProlog parses the given source and returns the predicates
// defined in the file. It first attempts to use the bundled Prolog
// parser (pl2ast.pl) via the `swipl` command to obtain a JSON AST.
// If that fails it falls back to a simple regex based parser.
func ParseProlog(src string) []PrologFunc {
	if funcs, err := parsePrologCLI(src); err == nil && len(funcs) > 0 {
		return funcs
	}
	var funcs []PrologFunc
	matches := prologDefRE.FindAllStringSubmatch(src, -1)
	for _, m := range matches {
		name := m[1]
		paramList := strings.TrimSpace(m[2])
		params := []string{}
		if paramList != "" {
			for _, p := range strings.Split(paramList, ",") {
				p = strings.TrimSpace(p)
				if p != "" {
					params = append(params, p)
				}
			}
		}
		body := strings.TrimSpace(m[3])
		if strings.HasSuffix(body, ".") {
			body = strings.TrimSuffix(body, ".")
		}
		funcs = append(funcs, PrologFunc{Name: name, Params: params, Body: body})
	}
	return funcs
}

// ParsePrologBody splits a Prolog predicate body into individual Mochi
// statements for a best-effort conversion.
func ParsePrologBody(body string) []string {
	clauses := splitClauses(body)
	var out []string
	for _, c := range clauses {
		c = strings.TrimSpace(c)
		switch {
		case c == "" || c == "true":
			continue
		case strings.HasPrefix(c, "write("):
			arg := strings.TrimSuffix(strings.TrimPrefix(c, "write("), ")")
			out = append(out, "  print("+arg+")")
		case c == "nl":
			// newline handled by print automatically
			continue
		case strings.Contains(c, " is "):
			parts := strings.SplitN(c, " is ", 2)
			name := strings.TrimSpace(parts[0])
			expr := strings.TrimSpace(parts[1])
			out = append(out, "  let "+name+" = "+expr)
		default:
			out = append(out, "  // "+c)
		}
	}
	if len(out) == 0 {
		out = append(out, "  pass")
	}
	return out
}

// splitClauses splits a Prolog body by commas at depth 0.
func splitClauses(body string) []string {
	var clauses []string
	depth := 0
	start := 0
	for i, r := range body {
		switch r {
		case '(':
			depth++
		case ')':
			if depth > 0 {
				depth--
			}
		case ',':
			if depth == 0 {
				clause := strings.TrimSpace(body[start:i])
				clauses = append(clauses, clause)
				start = i + 1
			}
		}
	}
	if start < len(body) {
		clauses = append(clauses, strings.TrimSpace(body[start:]))
	}
	return clauses
}

// parsePrologCLI invokes swipl with the bundled pl2ast.pl script to
// obtain a JSON AST for the given source. It returns any successfully
// parsed predicates.
func parsePrologCLI(src string) ([]PrologFunc, error) {
	script, err := os.CreateTemp("", "pl2ast-*.pl")
	if err != nil {
		return nil, err
	}
	defer os.Remove(script.Name())
	if _, err := script.Write(pl2astScript); err != nil {
		script.Close()
		return nil, err
	}
	script.Close()

	srcFile, err := os.CreateTemp("", "prolog-*.pl")
	if err != nil {
		return nil, err
	}
	if _, err := io.WriteString(srcFile, src); err != nil {
		srcFile.Close()
		os.Remove(srcFile.Name())
		return nil, err
	}
	srcFile.Close()
	defer os.Remove(srcFile.Name())

	cmd := exec.Command("swipl", "-q", "-f", script.Name(), "--", srcFile.Name())
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Run(); err != nil {
		return nil, err
	}

	var funcs []PrologFunc
	if err := json.Unmarshal(out.Bytes(), &funcs); err != nil {
		return nil, err
	}
	return funcs, nil
}
