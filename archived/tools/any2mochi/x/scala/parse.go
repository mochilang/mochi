//go:build slow

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

// Field describes a parameter or struct field with an optional type.
type Field struct {
	Name string `json:"name"`
	Type string `json:"type,omitempty"`
}

// Variant represents a case class belonging to a sealed trait.
type Variant struct {
	Name   string  `json:"name"`
	Fields []Field `json:"fields"`
}

// TypeDecl holds a trait name and its case class variants.
type TypeDecl struct {
	Name     string    `json:"name"`
	Variants []Variant `json:"variants"`
	Line     int       `json:"line,omitempty"`
	End      int       `json:"end,omitempty"`
}

// Func represents a top level Scala function extracted from the AST.
// Line and End store the start and end line numbers (1-based) of the
// function in the original source. ParamTypes holds optional type
// annotations extracted from the signature.
type Func struct {
	Name       string   `json:"name"`
	Params     []string `json:"params"`
	ParamTypes []string `json:"param_types,omitempty"`
	Return     string   `json:"return,omitempty"`
	Signature  string   `json:"signature,omitempty"`
	Body       []string `json:"body"`
	Line       int      `json:"line"`
	End        int      `json:"end"`
}

// File represents a parsed Scala source file.
type File struct {
	Types []TypeDecl `json:"types"`
	Funcs []Func     `json:"funcs"`
}

// runParse invokes the scalaast CLI to obtain the AST in JSON form.
func runParse(src string) (File, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	if _, err := exec.LookPath("scala-cli"); err == nil {
		cmd := exec.CommandContext(ctx, "scala-cli", "ast", "--format=json", "-")
		cmd.Stdin = bytes.NewBufferString(src)
		var out bytes.Buffer
		cmd.Stdout = &out
		if err := cmd.Run(); err == nil {
			var f File
			if jsonErr := json.Unmarshal(out.Bytes(), &f); jsonErr == nil {
				return f, nil
			}
		}
	}

	if _, err := exec.LookPath("scalac"); err == nil {
		tmp, err := os.CreateTemp("", "scala-src-*.scala")
		if err != nil {
			return File{}, err
		}
		defer os.Remove(tmp.Name())
		if _, err := tmp.WriteString(src); err != nil {
			return File{}, err
		}
		tmp.Close()

		cmd := exec.CommandContext(ctx, "scalac", "-Xfatal-warnings", "-Ystop-after:parser", tmp.Name())
		var out bytes.Buffer
		cmd.Stderr = &out
		if err := cmd.Run(); err != nil {
			return File{}, fmt.Errorf("scalac parse error:\n%s", out.String())
		}
		// scalac succeeded, fall through to simple parser
		return parseSource(src), nil
	}

	if _, err := exec.LookPath(parseCmd); err == nil {
		cmd := exec.CommandContext(ctx, parseCmd)
		cmd.Stdin = bytes.NewBufferString(src)
		var out bytes.Buffer
		cmd.Stdout = &out
		if err := cmd.Run(); err != nil {
			return File{}, err
		}
		var f File
		if err := json.Unmarshal(out.Bytes(), &f); err != nil {
			return File{}, err
		}
		return f, nil
	}

	// fallback: naive parser
	return parseSource(src), nil
}

func parseSource(src string) File {
	lines := strings.Split(strings.ReplaceAll(src, "\r\n", "\n"), "\n")
	reHeader := regexp.MustCompile(`^def\s+([a-zA-Z0-9_]+)\s*\(([^)]*)\)\s*(?:\:\s*([^=]+))?\s*=\s*(\{)?`)
	reTrait := regexp.MustCompile(`^sealed\s+trait\s+([A-Za-z0-9_]+)`)
	reCase := regexp.MustCompile(`^case\s+class\s+([A-Za-z0-9_]+)\s*\(([^)]*)\)\s*extends\s+([A-Za-z0-9_]+)`)
	depth := 0
	var file File
	for i := 0; i < len(lines); i++ {
		line := strings.TrimSpace(lines[i])
		if m := reTrait.FindStringSubmatch(line); m != nil {
			file.Types = append(file.Types, TypeDecl{Name: m[1], Line: i + 1})
			continue
		}
		if m := reCase.FindStringSubmatch(line); m != nil {
			typ := m[3]
			var fields []Field
			for _, p := range strings.Split(m[2], ",") {
				p = strings.TrimSpace(p)
				if p == "" {
					continue
				}
				name := p
				typStr := ""
				if idx := strings.Index(p, ":"); idx != -1 {
					name = strings.TrimSpace(p[:idx])
					typStr = strings.TrimSpace(p[idx+1:])
				}
				fields = append(fields, Field{Name: name, Type: typStr})
			}
			added := false
			for j := range file.Types {
				if file.Types[j].Name == typ {
					file.Types[j].Variants = append(file.Types[j].Variants, Variant{Name: m[1], Fields: fields})
					file.Types[j].End = i + 1
					added = true
					break
				}
			}
			if !added {
				file.Types = append(file.Types, TypeDecl{Name: typ, Variants: []Variant{{Name: m[1], Fields: fields}}, Line: i + 1, End: i + 1})
			}
			continue
		}
		if depth == 0 {
			if m := reHeader.FindStringSubmatch(line); m != nil {
				header := line
				name := m[1]
				params := []string{}
				types := []string{}
				for _, p := range strings.Split(m[2], ",") {
					p = strings.TrimSpace(p)
					if p == "" {
						continue
					}
					if idx := strings.Index(p, ":"); idx != -1 {
						types = append(types, strings.TrimSpace(p[idx+1:]))
						p = p[:idx]
					} else {
						types = append(types, "")
					}
					params = append(params, strings.TrimSpace(p))
				}
				ret := strings.TrimSpace(m[3])
				body := []string{}
				start := i + 1
				end := start
				if m[4] == "{" {
					depth = 1
					for i = i + 1; i < len(lines); i++ {
						l := strings.TrimSpace(lines[i])
						if strings.Contains(l, "{") {
							depth++
						}
						if strings.Contains(l, "}") {
							depth--
							if depth == 0 {
								end = i + 1
								break
							}
						}
						body = append(body, l)
					}
				} else {
					if idx := strings.Index(line, "="); idx != -1 {
						expr := strings.TrimSpace(line[idx+1:])
						body = append(body, expr)
					}
				}
				file.Funcs = append(file.Funcs, Func{Name: name, Params: params, ParamTypes: types, Return: ret, Signature: header, Body: body, Line: start, End: end})
			}
		}
	}
	return file
}
