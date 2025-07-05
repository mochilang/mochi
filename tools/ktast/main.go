package main

import (
	"bufio"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"regexp"
	"strings"
)

type Program struct {
	Functions []Function `json:"functions"`
}

type Function struct {
	Name   string   `json:"name"`
	Params []string `json:"params"`
	Lines  []string `json:"lines"`
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "usage: ktast <file>")
		os.Exit(1)
	}
	var data []byte
	var err error
	if os.Args[1] == "-" {
		data, err = io.ReadAll(os.Stdin)
	} else {
		data, err = os.ReadFile(os.Args[1])
	}
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	ast := parse(string(data))
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	if err := enc.Encode(ast); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

func parse(src string) Program {
	sc := bufio.NewScanner(strings.NewReader(src))
	funRE := regexp.MustCompile(`^fun\s+([A-Za-z0-9_]+)\s*\(([^)]*)\)`)
	var prog Program
	var cur *Function
	depth := 0
	for sc.Scan() {
		line := strings.TrimSpace(sc.Text())
		if cur == nil {
			if funRE.MatchString(line) {
				m := funRE.FindStringSubmatch(line)
				fn := Function{Name: m[1], Params: parseParams(m[2])}
				prog.Functions = append(prog.Functions, fn)
				cur = &prog.Functions[len(prog.Functions)-1]
				depth = strings.Count(line, "{") - strings.Count(line, "}")
				continue
			}
			continue
		}
		depth += strings.Count(line, "{")
		depth -= strings.Count(line, "}")
		if depth <= 0 && line == "}" {
			cur = nil
			depth = 0
			continue
		}
		cur.Lines = append(cur.Lines, line)
		if depth <= 0 {
			cur = nil
			depth = 0
		}
	}
	return prog
}

func parseParams(s string) []string {
	var out []string
	for _, p := range strings.Split(s, ",") {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		if i := strings.Index(p, ":"); i != -1 {
			p = p[:i]
		}
		out = append(out, strings.TrimSpace(p))
	}
	return out
}
