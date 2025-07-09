//go:build slow

package ts

import (
	"strings"

	parent "mochi/archived/tools/any2mochi"
)

// tsFunctionBody parses a subset of TypeScript statements and returns
// corresponding Mochi statements. Unsupported statements are ignored.
func tsFunctionBody(src string) []string {
	var lines []string
	uninit := map[string]bool{}
	s := strings.TrimSpace(src)
	for len(s) > 0 {
		// trim leading whitespace and semicolons
		s = strings.TrimLeft(s, " \t\n\r;")
		if len(s) == 0 {
			break
		}
		switch {
		case strings.HasPrefix(s, "break"):
			end := strings.Index(s, ";")
			if end == -1 {
				end = len(s)
			}
			lines = append(lines, "break")
			if end < len(s) {
				s = s[end+1:]
			} else {
				s = ""
			}
			continue
		case strings.HasPrefix(s, "continue"):
			end := strings.Index(s, ";")
			if end == -1 {
				end = len(s)
			}
			lines = append(lines, "continue")
			if end < len(s) {
				s = s[end+1:]
			} else {
				s = ""
			}
			continue
		case strings.HasPrefix(s, "return "):
			end := strings.Index(s, ";")
			if end == -1 {
				end = len(s)
			}
			expr := strings.TrimSpace(s[len("return "):end])
			lines = append(lines, "return "+expr)
			if end < len(s) {
				s = s[end+1:]
			} else {
				s = ""
			}
			continue
		case strings.HasPrefix(s, "console.log("):
			open := strings.Index(s, "(")
			end := findMatch(s, open, '(', ')')
			if end == -1 {
				end = len(s)
			}
			expr := strings.TrimSpace(s[len("console.log("):end])
			expr = strings.TrimSuffix(expr, ",")
			expr = strings.TrimSpace(expr)
			lines = append(lines, "print("+expr+")")
			sem := strings.Index(s[end:], ";")
			if sem != -1 {
				s = s[end+sem+1:]
			} else {
				s = s[end:]
			}
			continue
		case strings.HasPrefix(s, "let ") || strings.HasPrefix(s, "const ") || strings.HasPrefix(s, "var "):
			end := strings.Index(s, ";")
			if end == -1 {
				end = len(s)
			}
			stmt := strings.TrimSpace(s[:end])
			kw := "let"
			switch {
			case strings.HasPrefix(stmt, "var "):
				kw = "var"
				stmt = strings.TrimPrefix(stmt, "var ")
			case strings.HasPrefix(stmt, "let "):
				stmt = strings.TrimPrefix(stmt, "let ")
			case strings.HasPrefix(stmt, "const "):
				stmt = strings.TrimPrefix(stmt, "const ")
			}
			if eq := strings.Index(stmt, "="); eq != -1 {
				lines = append(lines, kw+" "+strings.TrimSpace(stmt))
			} else {
				uninit[strings.TrimSpace(stmt)] = true
			}
			if end < len(s) {
				s = s[end+1:]
			} else {
				s = ""
			}
			continue
		case strings.HasPrefix(s, "if ("):
			condEnd := findMatch(s, strings.Index(s, "("), '(', ')')
			cond := strings.TrimSpace(s[strings.Index(s, "(")+1 : condEnd])
			bodyStart := strings.Index(s[condEnd:], "{")
			if bodyStart == -1 {
				s = s[condEnd:]
				continue
			}
			bodyStart += condEnd + 1
			bodyEnd := findMatch(s, bodyStart-1, '{', '}')
			bodyLines := tsFunctionBody(s[bodyStart:bodyEnd])
			lines = append(lines, "if "+cond+" {")
			for _, l := range bodyLines {
				lines = append(lines, "  "+l)
			}
			lines = append(lines, "}")
			s = strings.TrimSpace(s[bodyEnd+1:])
			if strings.HasPrefix(s, "else {") {
				elseStart := strings.Index(s, "{") + 1
				elseEnd := findMatch(s, elseStart-1, '{', '}')
				elseLines := tsFunctionBody(s[elseStart:elseEnd])
				lines = append(lines, "else {")
				for _, l := range elseLines {
					lines = append(lines, "  "+l)
				}
				lines = append(lines, "}")
				s = s[elseEnd+1:]
			}
			continue
		case strings.HasPrefix(s, "for ("):
			parenEnd := findMatch(s, strings.Index(s, "("), '(', ')')
			clause := strings.TrimSpace(s[strings.Index(s, "(")+1 : parenEnd])
			if strings.Contains(clause, " of ") {
				parts := strings.SplitN(clause, " of ", 2)
				iter := strings.TrimSpace(parts[0])
				iter = strings.TrimPrefix(iter, "let ")
				iter = strings.TrimPrefix(iter, "const ")
				iter = strings.TrimPrefix(iter, "var ")
				list := strings.TrimSpace(parts[1])
				bodyStart := strings.Index(s[parenEnd:], "{")
				if bodyStart == -1 {
					s = s[parenEnd:]
					continue
				}
				bodyStart += parenEnd + 1
				bodyEnd := findMatch(s, bodyStart-1, '{', '}')
				bodyLines := tsFunctionBody(s[bodyStart:bodyEnd])
				lines = append(lines, "for "+iter+" in "+list+" {")
				for _, l := range bodyLines {
					lines = append(lines, "  "+l)
				}
				lines = append(lines, "}")
				s = s[bodyEnd+1:]
				continue
			}
			if idx := strings.Index(s, ";"); idx != -1 {
				s = s[idx+1:]
			} else {
				s = ""
			}
			continue
		case strings.HasPrefix(s, "while ("):
			parenEnd := findMatch(s, strings.Index(s, "("), '(', ')')
			cond := strings.TrimSpace(s[strings.Index(s, "(")+1 : parenEnd])
			bodyStart := strings.Index(s[parenEnd:], "{")
			if bodyStart == -1 {
				s = s[parenEnd:]
				continue
			}
			bodyStart += parenEnd + 1
			bodyEnd := findMatch(s, bodyStart-1, '{', '}')
			bodyLines := tsFunctionBody(s[bodyStart:bodyEnd])
			lines = append(lines, "while "+cond+" {")
			for _, l := range bodyLines {
				lines = append(lines, "  "+l)
			}
			lines = append(lines, "}")
			s = s[bodyEnd+1:]
			continue
		case strings.Contains(s, "="):
			end := strings.Index(s, ";")
			if end == -1 {
				end = len(s)
			}
			stmt := strings.TrimSpace(s[:end])
			parts := strings.SplitN(stmt, "=", 2)
			lhs := strings.TrimSpace(parts[0])
			rhs := strings.TrimSpace(parts[1])

			// Inline simple immediately invoked arrow functions.
			if strings.HasPrefix(rhs, "(() =>") {
				open := strings.Index(rhs, "{")
				close := findMatch(rhs, open, '{', '}')
				if open != -1 && close > open {
					body := rhs[open+1 : close]
					bodyLines := tsFunctionBody(body)
					ret := ""
					if len(bodyLines) > 0 {
						last := strings.TrimSpace(bodyLines[len(bodyLines)-1])
						if strings.HasPrefix(last, "return ") {
							ret = strings.TrimSpace(strings.TrimPrefix(last, "return "))
							bodyLines = bodyLines[:len(bodyLines)-1]
						}
					}
					for _, l := range bodyLines {
						lines = append(lines, l)
					}
					if ret != "" {
						if uninit[lhs] {
							lines = append(lines, "var "+lhs+" = "+ret)
							delete(uninit, lhs)
						} else {
							lines = append(lines, lhs+" = "+ret)
						}
					}
					if end < len(s) {
						s = s[end+1:]
					} else {
						s = ""
					}
					continue
				}
			}

			expr := strings.ReplaceAll(rhs, "\n", " ")
			if strings.Contains(expr, ".filter(") && strings.Contains(expr, ".map(") {
				if out := parseFilterMap(expr, lhs); len(out) > 0 {
					lines = append(lines, out...)
				}
			} else {
				if uninit[lhs] {
					lines = append(lines, "var "+lhs+" = "+expr)
					delete(uninit, lhs)
				} else {
					lines = append(lines, lhs+" = "+expr)
				}
			}
			if end < len(s) {
				s = s[end+1:]
			} else {
				s = ""
			}
			continue
		default:
			if idx := strings.Index(s, ";"); idx != -1 {
				s = s[idx+1:]
			} else {
				s = ""
			}
		}
	}
	return lines
}

// indexForPosition converts a protocol position to a byte offset in src.
func indexForPosition(src string, pos parent.Position) int {
	lines := strings.Split(src, "\n")
	if int(pos.Line) >= len(lines) {
		return len(src)
	}
	idx := 0
	for i := 0; i < int(pos.Line); i++ {
		idx += len(lines[i]) + 1
	}
	if int(pos.Character) > len(lines[int(pos.Line)]) {
		idx += len(lines[int(pos.Line)])
	} else {
		idx += int(pos.Character)
	}
	return idx
}

// findMatch returns the index of the matching closing delimiter for the opening
// delimiter at openIdx. If no match is found it returns len(s).
func findMatch(s string, openIdx int, open, close rune) int {
	depth := 0
	for i, r := range s[openIdx:] {
		if r == open {
			depth++
		} else if r == close {
			depth--
			if depth == 0 {
				return openIdx + i
			}
		}
	}
	return len(s)
}

func parseFilterMap(expr, lhs string) []string {
	filterIdx := strings.Index(expr, ".filter(")
	mapIdx := strings.Index(expr, ".map(")
	if filterIdx == -1 || mapIdx == -1 || mapIdx < filterIdx {
		return nil
	}
	list := strings.TrimSpace(expr[:filterIdx])
	fStart := filterIdx + len(".filter(")
	fEnd := findMatch(expr, fStart-1, '(', ')')
	if fEnd <= fStart {
		return nil
	}
	fPart := expr[fStart:fEnd]
	arrow := strings.Index(fPart, "=>")
	if arrow == -1 {
		return nil
	}
	iter := strings.TrimSpace(strings.Trim(fPart[:arrow], "() "))
	cond := strings.TrimSpace(strings.Trim(fPart[arrow+2:], "() "))
	mStart := mapIdx + len(".map(")
	mEnd := findMatch(expr, mStart-1, '(', ')')
	if mEnd <= mStart {
		return nil
	}
	mPart := expr[mStart:mEnd]
	arrow = strings.Index(mPart, "=>")
	if arrow == -1 {
		return nil
	}
	iter2 := strings.TrimSpace(strings.Trim(mPart[:arrow], "() "))
	body := strings.TrimSpace(strings.Trim(mPart[arrow+2:], "() "))
	if iter2 != "" {
		iter = iter2
	}
	var out []string
	out = append(out, lhs+" = from "+iter+" in "+list)
	out = append(out, "             where "+cond)
	out = append(out, "             select "+body)
	return out
}
