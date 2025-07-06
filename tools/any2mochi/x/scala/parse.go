package scala

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

// parseCmd is the CLI used to parse Scala source into a simple AST.
var parseCmd = "scalaast"

// Func represents a top level Scala function extracted from the AST.
// Line stores the line number where the function starts in the source.
// Return holds the declared return type if present.
type Func struct {
	Name   string   `json:"name"`
	Params []string `json:"params"`
	Return string   `json:"return,omitempty"`
	Body   []string `json:"body"`
	Line   int      `json:"line"`
}

// runParse invokes the scalaast CLI to obtain the AST in JSON form.
func runParse(src string) ([]Func, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	if _, err := exec.LookPath(parseCmd); err == nil {
		cmd := exec.CommandContext(ctx, parseCmd)
		cmd.Stdin = bytes.NewBufferString(src)
		var out bytes.Buffer
		cmd.Stdout = &out
		if err := cmd.Run(); err != nil {
			return nil, err
		}
		var funcs []Func
		if err := json.Unmarshal(out.Bytes(), &funcs); err != nil {
			return nil, err
		}
		return funcs, nil
	}

	tmp, err := os.CreateTemp("", "scala-src-*.scala")
	if err != nil {
		return nil, err
	}
	defer os.Remove(tmp.Name())
	if _, err := tmp.WriteString(src); err != nil {
		return nil, err
	}
	tmp.Close()

	cmd := exec.CommandContext(ctx, "scalac", "-Xfatal-warnings", "-Ystop-after:parser", tmp.Name())
	var out bytes.Buffer
	cmd.Stderr = &out
	if err := cmd.Run(); err != nil {
		return nil, fmt.Errorf("scalac parse error:\n%s", out.String())
	}

	funcs := parseSource(src)
	return funcs, nil
}

func parseSource(src string) []Func {
	lines := strings.Split(strings.ReplaceAll(src, "\r\n", "\n"), "\n")
	reHeader := regexp.MustCompile(`^def\s+([a-zA-Z0-9_]+)\s*\(([^)]*)\)\s*(?::\s*([^=]+))?=\s*{`)
	depth := 0
	var funcs []Func
	for i := 0; i < len(lines); i++ {
		line := strings.TrimSpace(lines[i])
		if depth == 0 {
			if m := reHeader.FindStringSubmatch(line); m != nil {
				name := m[1]
				params := []string{}
				for _, p := range strings.Split(m[2], ",") {
					p = strings.TrimSpace(p)
					if p == "" {
						continue
					}
					if idx := strings.Index(p, ":"); idx != -1 {
						p = p[:idx]
					}
					params = append(params, strings.TrimSpace(p))
				}
				ret := strings.TrimSpace(m[3])
				body := []string{}
				depth = 1
				start := i + 1
				for i = i + 1; i < len(lines); i++ {
					l := strings.TrimSpace(lines[i])
					if strings.Contains(l, "{") {
						depth++
					}
					if strings.Contains(l, "}") {
						depth--
						if depth == 0 {
							break
						}
					}
					body = append(body, l)
				}
				funcs = append(funcs, Func{Name: name, Params: params, Return: ret, Body: body, Line: start})
			}
		}
	}
	return funcs
}
