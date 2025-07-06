package ts

import (
	"strings"

	a2m "mochi/tools/any2mochi"
)

// typeScriptFunctionBody parses a subset of TypeScript statements and returns
// corresponding Mochi statements. Unsupported statements are ignored.
func typeScriptFunctionBody(src string) []string {
	var lines []string
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
			end := strings.Index(s, ")")
			if end == -1 {
				end = len(s)
			}
			expr := strings.TrimSpace(s[len("console.log("):end])
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
			stmt = strings.TrimPrefix(stmt, "let ")
			stmt = strings.TrimPrefix(stmt, "const ")
			stmt = strings.TrimPrefix(stmt, "var ")
			lines = append(lines, "let "+strings.TrimSuffix(stmt, ";"))
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
			bodyLines := typeScriptFunctionBody(s[bodyStart:bodyEnd])
			lines = append(lines, "if "+cond+" {")
			for _, l := range bodyLines {
				lines = append(lines, "  "+l)
			}
			lines = append(lines, "}")
			s = strings.TrimSpace(s[bodyEnd+1:])
			if strings.HasPrefix(s, "else {") {
				elseStart := strings.Index(s, "{") + 1
				elseEnd := findMatch(s, elseStart-1, '{', '}')
				elseLines := typeScriptFunctionBody(s[elseStart:elseEnd])
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
				bodyLines := typeScriptFunctionBody(s[bodyStart:bodyEnd])
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
			bodyLines := typeScriptFunctionBody(s[bodyStart:bodyEnd])
			lines = append(lines, "while "+cond+" {")
			for _, l := range bodyLines {
				lines = append(lines, "  "+l)
			}
			lines = append(lines, "}")
			s = s[bodyEnd+1:]
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
func indexForPosition(src string, pos a2m.Position) int {
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
