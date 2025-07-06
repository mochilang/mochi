package main

import (
	"encoding/json"
	"fmt"
	"io"
	"os"
	"strings"
	"unicode"
)

type item struct {
	Kind   string   `json:"kind"`
	Name   string   `json:"name"`
	Params []string `json:"params,omitempty"`
	Body   string   `json:"body,omitempty"`
	Value  string   `json:"value,omitempty"`
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

func parseRacket(src string) []item {
	toks := tokenize(src)
	return parseTokens(toks)
}

func main() {
	data, err := io.ReadAll(os.Stdin)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	items := parseRacket(string(data))
	enc := json.NewEncoder(os.Stdout)
	if err := enc.Encode(items); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}
