//go:build ignore
// +build ignore

package main

import (
	"fmt"
	"strings"
	"unicode"
)

func snakeToCamel(s string) string {
	s = strings.TrimSpace(s)
	var out []rune
	upper := false
	for i, r := range s {
		switch r {
		case '_', '-', ' ', '.':
			upper = true
			continue
		}
		if i == 0 {
			out = append(out, unicode.ToLower(r))
			upper = false
			continue
		}
		if upper {
			out = append(out, unicode.ToUpper(r))
			upper = false
		} else {
			out = append(out, r)
		}
	}
	return string(out)
}

func camelToSnake(s string) string {
	s = strings.TrimSpace(s)
	var b strings.Builder
	prevUnd := false
	for i, r := range s {
		switch r {
		case ' ', '-', '.':
			if !prevUnd && b.Len() > 0 {
				b.WriteByte('_')
				prevUnd = true
			}
			continue
		case '_':
			if !prevUnd && b.Len() > 0 {
				b.WriteRune(r)
				prevUnd = true
			}
			continue
		}
		if unicode.IsUpper(r) {
			if i > 0 && !prevUnd {
				b.WriteByte('_')
			}
			b.WriteRune(unicode.ToLower(r))
			prevUnd = false
		} else {
			b.WriteRune(unicode.ToLower(r))
			prevUnd = false
		}
	}
	res := b.String()
	res = strings.Trim(res, "_")
	for strings.Contains(res, "__") {
		res = strings.ReplaceAll(res, "__", "_")
	}
	return res
}

func main() {
	samples := []string{
		"snakeCase",
		"snake_case",
		"snake-case",
		"snake case",
		"snake CASE",
		"snake.case",
		"variable_10_case",
		"variable10Case",
		"ɛrgo rE tHis",
		"hurry-up-joe!",
		"c://my-docs/happy_Flag-Day/12.doc",
		" spaces ",
	}
	fmt.Println("=== To snake_case ===")
	for _, s := range samples {
		fmt.Printf("%34s => %s\n", s, camelToSnake(s))
	}
	fmt.Println()
	fmt.Println("=== To camelCase ===")
	for _, s := range samples {
		fmt.Printf("%34s => %s\n", s, snakeToCamel(s))
	}
}
