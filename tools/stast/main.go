package main

import (
	"encoding/json"
	"fmt"
	"io"
	"os"
	"regexp"
	"strings"
)

type Program struct {
	Statements []Stmt `json:"statements"`
}

type Stmt struct {
	Kind        string `json:"kind"`
	Name        string `json:"name,omitempty"`
	Expr        string `json:"expr,omitempty"`
	Cond        string `json:"cond,omitempty"`
	Start       string `json:"start,omitempty"`
	End         string `json:"end,omitempty"`
	Collection  string `json:"collection,omitempty"`
	Body        []Stmt `json:"body,omitempty"`
	Else        []Stmt `json:"else,omitempty"`
	Line        int    `json:"line,omitempty"`
	Column      int    `json:"column,omitempty"`
	EndLine     int    `json:"endLine,omitempty"`
	EndColumn   int    `json:"endColumn,omitempty"`
	StartOffset int    `json:"startOffset,omitempty"`
	EndOffset   int    `json:"endOffset,omitempty"`
	Snippet     string `json:"snippet,omitempty"`
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "usage: stast <file>")
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
	prog := parse(string(data))
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	if err := enc.Encode(prog); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

func parse(src string) Program {
	lines := strings.Split(src, "\n")
	offsets := make([]int, len(lines)+1)
	pos := 0
	for i, l := range lines {
		offsets[i] = pos
		pos += len(l) + 1
	}
	offsets[len(lines)] = pos
	start := -1
	for i, l := range lines {
		if strings.HasPrefix(strings.TrimSpace(l), "!!") {
			start = i + 1
			break
		}
	}
	if start == -1 {
		start = 0
	}
	var stmts []Stmt
	for i := start; i < len(lines); i++ {
		l := strings.TrimSpace(lines[i])
		if l == "" || l == "." {
			continue
		}
		if stmt, n := parseWhile(lines, i, offsets); n > 0 {
			stmt.Line = i + 1
			stmt.Column = len(lines[i]) - len(strings.TrimLeft(lines[i], " \t")) + 1
			stmt.EndLine = i + n - 1
			stmt.EndColumn = len(strings.TrimRight(lines[i+n-1], "\n"))
			stmt.StartOffset = offsets[i] + stmt.Column - 1
			stmt.EndOffset = offsets[i+n-1] + stmt.EndColumn
			stmt.Snippet = strings.TrimSpace(strings.Join(lines[i:i+n], "\n"))
			stmts = append(stmts, stmt)
			i += n - 1
			continue
		}
		if stmt, n := parseIf(lines, i, offsets); n > 0 {
			stmt.Line = i + 1
			stmt.Column = len(lines[i]) - len(strings.TrimLeft(lines[i], " \t")) + 1
			stmt.EndLine = i + n - 1
			stmt.EndColumn = len(strings.TrimRight(lines[i+n-1], "\n"))
			stmt.StartOffset = offsets[i] + stmt.Column - 1
			stmt.EndOffset = offsets[i+n-1] + stmt.EndColumn
			stmt.Snippet = strings.TrimSpace(strings.Join(lines[i:i+n], "\n"))
			stmts = append(stmts, stmt)
			i += n - 1
			continue
		}
		if stmt, n := parseFor(lines, i, offsets); n > 0 {
			stmt.Line = i + 1
			stmt.Column = len(lines[i]) - len(strings.TrimLeft(lines[i], " \t")) + 1
			stmt.EndLine = i + n - 1
			stmt.EndColumn = len(strings.TrimRight(lines[i+n-1], "\n"))
			stmt.StartOffset = offsets[i] + stmt.Column - 1
			stmt.EndOffset = offsets[i+n-1] + stmt.EndColumn
			stmt.Snippet = strings.TrimSpace(strings.Join(lines[i:i+n], "\n"))
			stmts = append(stmts, stmt)
			i += n - 1
			continue
		}
		s := parseSimpleStmt(strings.TrimSuffix(l, "."))
		if s.Kind != "" {
			s.Line = i + 1
			s.Column = len(lines[i]) - len(strings.TrimLeft(lines[i], " \t")) + 1
			s.EndLine = i + 1
			s.EndColumn = len(strings.TrimRight(lines[i], "\n"))
			s.StartOffset = offsets[i] + s.Column - 1
			s.EndOffset = offsets[i] + s.EndColumn
			if s.Snippet == "" {
				s.Snippet = strings.TrimSpace(lines[i])
			}
			stmts = append(stmts, s)
		} else {
			stmts = append(stmts, Stmt{Kind: "unknown", Expr: l, Line: i + 1, Column: 1, StartOffset: offsets[i]})
		}
	}
	return Program{Statements: stmts}
}

func parseSimpleStmt(line string) Stmt {
	trimmed := strings.TrimLeft(line, " \t")
	col := len(line) - len(trimmed) + 1
	l := strings.TrimSpace(trimmed)
	if strings.HasSuffix(l, "Transcript cr") {
		l = strings.TrimSuffix(l, "Transcript cr")
		l = strings.TrimSpace(strings.TrimSuffix(l, "."))
	}
	if strings.Contains(l, "displayOn: Transcript") {
		parts := strings.Split(l, "displayOn: Transcript")
		expr := strings.TrimSpace(parts[0])
		expr = strings.Trim(expr, "()")
		expr = strings.ReplaceAll(expr, "'", "\"")
		return Stmt{Kind: "print", Expr: expr, Column: col, EndLine: 0, EndColumn: len(line), Snippet: strings.TrimSpace(line)}
	}
	if strings.Contains(l, ":=") {
		parts := strings.SplitN(l, ":=", 2)
		left := strings.TrimSpace(parts[0])
		right := strings.TrimSpace(strings.TrimSuffix(parts[1], "."))
		return Stmt{Kind: "assign", Name: left, Expr: right, Column: col, EndLine: 0, EndColumn: len(line), Snippet: strings.TrimSpace(line)}
	}
	if strings.HasPrefix(l, "^") {
		return Stmt{Kind: "return", Expr: strings.TrimSpace(strings.TrimPrefix(l, "^")), Column: col, EndLine: 0, EndColumn: len(line), Snippet: strings.TrimSpace(line)}
	}
	if m := regexp.MustCompile(`^Transcript\s+show:`).FindString(l); m != "" {
		expr := strings.TrimSpace(strings.TrimPrefix(l, m))
		expr = strings.TrimSuffix(expr, ".")
		expr = strings.Trim(expr, "()")
		expr = strings.ReplaceAll(expr, "'", "\"")
		return Stmt{Kind: "print", Expr: expr, Column: col, EndLine: 0, EndColumn: len(line), Snippet: strings.TrimSpace(line)}
	}
	if l != "" {
		return Stmt{Kind: "unknown", Expr: l, Column: col, EndLine: 0, EndColumn: len(line), Snippet: strings.TrimSpace(line)}
	}
	return Stmt{}
}

func parseWhile(lines []string, i int, offsets []int) (Stmt, int) {
	l := strings.TrimSpace(lines[i])
	if !strings.Contains(l, "whileTrue:") {
		return Stmt{}, 0
	}
	m := regexp.MustCompile(`^\[(.*)\]\s+whileTrue:\s+\[$`).FindStringSubmatch(l)
	if len(m) != 2 {
		return Stmt{}, 0
	}
	cond := strings.TrimSpace(m[1])
	var body []Stmt
	n := 1
	for j := i + 1; j < len(lines); j++ {
		raw := lines[j]
		line := strings.TrimSpace(raw)
		if line == "]" {
			n = j - i + 1
			break
		}
		if line == "" || line == "." {
			continue
		}
		stmt := parseSimpleStmt(strings.TrimSuffix(raw, "."))
		if stmt.Kind != "" {
			stmt.Line = j + 1
			stmt.Column = len(raw) - len(strings.TrimLeft(raw, " \t")) + 1
			stmt.EndLine = j + 1
			stmt.EndColumn = len(strings.TrimRight(raw, "\n"))
			stmt.StartOffset = offsets[j] + stmt.Column - 1
			stmt.EndOffset = offsets[j] + stmt.EndColumn
			if stmt.Snippet == "" {
				stmt.Snippet = strings.TrimSpace(raw)
			}
			body = append(body, stmt)
		} else {
			body = append(body, Stmt{Kind: "unknown", Expr: line, Line: j + 1, Column: 1, StartOffset: offsets[j]})
		}
	}
	snippet := strings.TrimSpace(strings.Join(lines[i:i+n], "\n"))
	col := len(lines[i]) - len(strings.TrimLeft(lines[i], " \t")) + 1
	return Stmt{Kind: "while", Cond: cond, Body: body, Column: col, Snippet: snippet, StartOffset: offsets[i] + col - 1, EndLine: i + n - 1, EndColumn: len(strings.TrimRight(lines[i+n-1], "\n")), EndOffset: offsets[i+n-1] + len(strings.TrimRight(lines[i+n-1], "\n"))}, n
}

func parseFor(lines []string, i int, offsets []int) (Stmt, int) {
	l := strings.TrimSpace(lines[i])
	if !strings.Contains(l, " do:") {
		return Stmt{}, 0
	}
	if m := regexp.MustCompile(`^(.*)\s+to:\s+(.*)\s+do:\s+\[:([A-Za-z_][A-Za-z0-9_]*) \|$`).FindStringSubmatch(l); len(m) == 4 {
		start := strings.TrimSpace(m[1])
		end := strings.TrimSpace(m[2])
		if strings.HasSuffix(end, "- 1") {
			end = strings.TrimSpace(strings.TrimSuffix(end, "- 1"))
		}
		name := strings.TrimSpace(m[3])
		body, n := parseBlock(lines, i, offsets)
		snippet := strings.TrimSpace(strings.Join(lines[i:i+n], "\n"))
		col := len(lines[i]) - len(strings.TrimLeft(lines[i], " \t")) + 1
		return Stmt{Kind: "for", Name: name, Start: start, End: end, Body: body, Column: col, Snippet: snippet, StartOffset: offsets[i] + col - 1, EndLine: i + n - 1, EndColumn: len(strings.TrimRight(lines[i+n-1], "\n")), EndOffset: offsets[i+n-1] + len(strings.TrimRight(lines[i+n-1], "\n"))}, n
	}
	m := regexp.MustCompile(`^(.*)\s+do:\s+\[:([A-Za-z_][A-Za-z0-9_]*) \|$`).FindStringSubmatch(l)
	if len(m) != 3 {
		return Stmt{}, 0
	}
	coll := strings.TrimSpace(m[1])
	name := strings.TrimSpace(m[2])
	body, n := parseBlock(lines, i, offsets)
	snippet := strings.TrimSpace(strings.Join(lines[i:i+n], "\n"))
	col := len(lines[i]) - len(strings.TrimLeft(lines[i], " \t")) + 1
	return Stmt{Kind: "foreach", Name: name, Collection: coll, Body: body, Column: col, Snippet: snippet, StartOffset: offsets[i] + col - 1, EndLine: i + n - 1, EndColumn: len(strings.TrimRight(lines[i+n-1], "\n")), EndOffset: offsets[i+n-1] + len(strings.TrimRight(lines[i+n-1], "\n"))}, n
}

func parseBlock(lines []string, i int, offsets []int) ([]Stmt, int) {
	var body []Stmt
	n := 1
	for j := i + 1; j < len(lines); j++ {
		raw := lines[j]
		line := strings.TrimSpace(raw)
		if line == "]" {
			n = j - i + 1
			break
		}
		if line == "" || line == "." {
			continue
		}
		stmt := parseSimpleStmt(strings.TrimSuffix(raw, "."))
		if stmt.Kind != "" {
			stmt.Line = j + 1
			stmt.Column = len(raw) - len(strings.TrimLeft(raw, " \t")) + 1
			stmt.EndLine = j + 1
			stmt.EndColumn = len(strings.TrimRight(raw, "\n"))
			stmt.StartOffset = offsets[j] + stmt.Column - 1
			stmt.EndOffset = offsets[j] + stmt.EndColumn
			if stmt.Snippet == "" {
				stmt.Snippet = strings.TrimSpace(raw)
			}
			body = append(body, stmt)
		} else {
			body = append(body, Stmt{Kind: "unknown", Expr: line, Line: j + 1, Column: 1, StartOffset: offsets[j]})
		}
	}
	return body, n
}

func parseIf(lines []string, i int, offsets []int) (Stmt, int) {
	l := strings.TrimSpace(lines[i])
	if !strings.Contains(l, "ifTrue:") {
		return Stmt{}, 0
	}
	m := regexp.MustCompile(`^(.*)\s+ifTrue:\s+\[$`).FindStringSubmatch(l)
	if len(m) != 2 {
		return Stmt{}, 0
	}
	cond := strings.TrimSpace(strings.Trim(m[1], "()"))
	thenBody, n := parseBlock(lines, i, offsets)
	j := i + n
	var elseBody []Stmt
	var m2 int
	// handle pattern where "ifFalse:" appears on the same line as the closing bracket
	if j-3 >= 0 && strings.Contains(lines[j-3], "ifFalse:") {
		if len(thenBody) > 0 && thenBody[len(thenBody)-1].Kind == "print" {
			// remove trailing statement belonging to else block if detected
			thenBody = thenBody[:len(thenBody)-1]
		}
		if len(thenBody) > 0 && thenBody[len(thenBody)-1].Kind == "unknown" {
			thenBody = thenBody[:len(thenBody)-1]
		}
		elseBody, m2 = parseBlock(lines, j-3, offsets)
		snippet := strings.TrimSpace(strings.Join(lines[i:i+n+m2-3], "\n"))
		col := len(lines[i]) - len(strings.TrimLeft(lines[i], " \t")) + 1
		return Stmt{Kind: "if", Cond: cond, Body: thenBody, Else: elseBody, Column: col, Snippet: snippet, StartOffset: offsets[i] + col - 1, EndLine: i + n + m2 - 4, EndColumn: len(strings.TrimRight(lines[i+n+m2-4], "\n")), EndOffset: offsets[i+n+m2-4] + len(strings.TrimRight(lines[i+n+m2-4], "\n"))}, n + m2 - 3
	}
	// handle pattern "] ifFalse: [" on its own line
	if strings.Contains(lines[j-1], "ifFalse:") {
		elseBody, m2 = parseBlock(lines, j-1, offsets)
		snippet := strings.TrimSpace(strings.Join(lines[i:i+n+m2-1], "\n"))
		col := len(lines[i]) - len(strings.TrimLeft(lines[i], " \t")) + 1
		return Stmt{Kind: "if", Cond: cond, Body: thenBody, Else: elseBody, Column: col, Snippet: snippet, StartOffset: offsets[i] + col - 1, EndLine: i + n + m2 - 2, EndColumn: len(strings.TrimRight(lines[i+n+m2-2], "\n")), EndOffset: offsets[i+n+m2-2] + len(strings.TrimRight(lines[i+n+m2-2], "\n"))}, n + m2 - 1
	}
	// handle next line starting with ifFalse:
	if j < len(lines) && strings.HasPrefix(strings.TrimSpace(lines[j]), "ifFalse:") {
		elseBody, m2 = parseBlock(lines, j, offsets)
		snippet := strings.TrimSpace(strings.Join(lines[i:i+n+m2], "\n"))
		col := len(lines[i]) - len(strings.TrimLeft(lines[i], " \t")) + 1
		return Stmt{Kind: "if", Cond: cond, Body: thenBody, Else: elseBody, Column: col, Snippet: snippet, StartOffset: offsets[i] + col - 1, EndLine: i + n + m2 - 1, EndColumn: len(strings.TrimRight(lines[i+n+m2-1], "\n")), EndOffset: offsets[i+n+m2-1] + len(strings.TrimRight(lines[i+n+m2-1], "\n"))}, n + m2
	}
	snippet := strings.TrimSpace(strings.Join(lines[i:i+n], "\n"))
	col := len(lines[i]) - len(strings.TrimLeft(lines[i], " \t")) + 1
	return Stmt{Kind: "if", Cond: cond, Body: thenBody, Column: col, Snippet: snippet, StartOffset: offsets[i] + col - 1, EndLine: i + n - 1, EndColumn: len(strings.TrimRight(lines[i+n-1], "\n")), EndOffset: offsets[i+n-1] + len(strings.TrimRight(lines[i+n-1], "\n"))}, n
}
