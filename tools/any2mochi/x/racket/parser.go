package racket

import (
	"bytes"
	"encoding/json"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"unicode"
)

type item struct {
	Kind      string   `json:"kind"`
	Name      string   `json:"name"`
	Params    []string `json:"params,omitempty"`
	Body      string   `json:"body,omitempty"`
	Value     string   `json:"value,omitempty"`
	StartLine int      `json:"start_line,omitempty"`
	EndLine   int      `json:"end_line,omitempty"`
}

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
		case unicode.IsSpace(r):
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
			i++ // skip ')'
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
				i++ // skip ')'
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
				i++ // skip ')'
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

func parseOfficial(src string) ([]item, error) {
	scriptPath := filepath.Join(filepath.Dir(currentFile()), "read_ast.rkt")
	cmd := exec.Command("racket", scriptPath)
	cmd.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Run(); err != nil {
		return nil, err
	}
	var node map[string]interface{}
	if err := json.Unmarshal(out.Bytes(), &node); err != nil {
		return nil, err
	}
	var items []item
	if datum, ok := node["datum"].([]interface{}); ok {
		for _, v := range datum {
			if m, ok := v.(map[string]interface{}); ok {
				items = append(items, walkSyntax(src, m)...)
			}
		}
	}
	return items, nil
}

func parse(src string) []item {
	if items, err := parseOfficial(src); err == nil && len(items) > 0 {
		return items
	}
	toks := tokenize(src)
	return parseTokens(toks)
}

func currentFile() string {
	_, file, _, _ := runtime.Caller(0)
	return file
}

func walkSyntax(src string, node map[string]interface{}) []item {
	datum, ok := node["datum"].([]interface{})
	if !ok || len(datum) == 0 {
		return nil
	}
	first, _ := datum[0].(map[string]interface{})
	if first == nil {
		return nil
	}
	fn, _ := first["datum"].(string)
	start, _ := node["pos"].(float64)
	span, _ := node["span"].(float64)
	startLine := lineForPos(src, int(start))
	endLine := lineForPos(src, int(start+span))

	switch fn {
	case "define":
		if len(datum) >= 3 {
			if nameList, ok := datum[1].(map[string]interface{}); ok {
				if inner, ok := nameList["datum"].([]interface{}); ok && len(inner) > 0 {
					// function definition
					nm, _ := inner[0].(map[string]interface{})
					fname, _ := nm["datum"].(string)
					var params []string
					for _, p := range inner[1:] {
						if pm, ok := p.(map[string]interface{}); ok {
							if ps, ok := pm["datum"].(string); ok {
								params = append(params, ps)
							}
						}
					}
					return []item{{Kind: "func", Name: fname, Params: params, Body: "", StartLine: startLine, EndLine: endLine}}
				}
			}
			// variable definition
			if nameNode, ok := datum[1].(map[string]interface{}); ok {
				nm, _ := nameNode["datum"].(string)
				return []item{{Kind: "var", Name: nm, Value: "", StartLine: startLine, EndLine: endLine}}
			}
		}
	}

	return nil
}

func lineForPos(src string, pos int) int {
	if pos <= 0 {
		return 1
	}
	cnt := 1
	for i := 0; i < len(src) && i < pos; i++ {
		if src[i] == '\n' {
			cnt++
		}
	}
	return cnt
}
