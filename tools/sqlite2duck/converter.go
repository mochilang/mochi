package sqlite2duck

import (
	"strings"
	"unicode"
)

var funcConversions = []struct{ in, out string }{
	{"ifnull", "coalesce("},
	{"substr", "substring("},
	{"group_concat", "string_agg("},
	{"randomblob", "random_bytes("},
	{"char_length", "length("},
	{"character_length", "length("},
	{"printf", "format("},
	{"instr", "strpos("},
	{"current_timestamp", "now("},
	{"current_date", "current_date("},
	{"current_time", "current_time("},
}

var wordConversions = []struct{ in, out string }{
	{"current_timestamp", "now()"},
	{"current_date", "current_date"},
	{"current_time", "current_time"},
	{"true", "true"},
	{"false", "false"},
}

const (
	dtNow   = "datetime('now'"
	dNow    = "date('now'"
	tNow    = "time('now'"
	jdNow   = "julianday('now'"
	sfNow   = "strftime("
	notIdx  = " not indexed"
	totalFn = "total"
)

func isIdentChar(r byte) bool {
	return r == '_' || (r >= 'a' && r <= 'z') || (r >= 'A' && r <= 'Z') || (r >= '0' && r <= '9')
}

func replaceFunc(sql, name, repl string) string {
	lower := strings.ToLower(sql)
	var out strings.Builder
	i := 0
	for i < len(sql) {
		idx := strings.Index(lower[i:], name)
		if idx == -1 {
			out.WriteString(sql[i:])
			break
		}
		idx += i
		out.WriteString(sql[i:idx])
		if idx > 0 && isIdentChar(lower[idx-1]) {
			out.WriteString(sql[idx : idx+1])
			i = idx + 1
			continue
		}
		j := idx + len(name)
		if j < len(sql) && isIdentChar(lower[j]) {
			out.WriteString(sql[idx:j])
			i = j
			continue
		}
		k := j
		for k < len(sql) && unicode.IsSpace(rune(sql[k])) {
			k++
		}
		if k < len(sql) && sql[k] == '(' {
			out.WriteString(repl)
			i = k + 1
			continue
		}
		out.WriteString(sql[idx:j])
		i = j
	}
	return out.String()
}

func replaceWord(sql, word, repl string) string {
	lower := strings.ToLower(sql)
	var out strings.Builder
	i := 0
	for i < len(sql) {
		idx := strings.Index(lower[i:], word)
		if idx == -1 {
			out.WriteString(sql[i:])
			break
		}
		idx += i
		out.WriteString(sql[i:idx])
		if idx > 0 && isIdentChar(lower[idx-1]) {
			out.WriteString(sql[idx : idx+len(word)])
			i = idx + len(word)
			continue
		}
		end := idx + len(word)
		if end < len(sql) && isIdentChar(lower[end]) {
			out.WriteString(sql[idx:end])
			i = end
			continue
		}
		out.WriteString(repl)
		i = end
	}
	return out.String()
}

func replaceDateTime(sql string) string {
	lower := strings.ToLower(sql)
	for {
		idx := strings.Index(lower, dtNow)
		if idx == -1 {
			break
		}
		end := idx + len(dtNow)
		for end < len(sql) && sql[end] != ')' {
			end++
		}
		if end < len(sql) {
			sql = sql[:idx] + "now()" + sql[end+1:]
			lower = strings.ToLower(sql)
			continue
		}
		break
	}
	return sql
}

func replaceDate(sql string) string {
	lower := strings.ToLower(sql)
	for {
		idx := strings.Index(lower, dNow)
		if idx == -1 {
			break
		}
		end := idx + len(dNow)
		for end < len(sql) && sql[end] != ')' {
			end++
		}
		if end < len(sql) {
			sql = sql[:idx] + "current_date" + sql[end+1:]
			lower = strings.ToLower(sql)
			continue
		}
		break
	}
	return sql
}

func replaceTime(sql string) string {
	lower := strings.ToLower(sql)
	for {
		idx := strings.Index(lower, tNow)
		if idx == -1 {
			break
		}
		end := idx + len(tNow)
		for end < len(sql) && sql[end] != ')' {
			end++
		}
		if end < len(sql) {
			sql = sql[:idx] + "current_time" + sql[end+1:]
			lower = strings.ToLower(sql)
			continue
		}
		break
	}
	return sql
}

func replaceJulianday(sql string) string {
	lower := strings.ToLower(sql)
	for {
		idx := strings.Index(lower, jdNow)
		if idx == -1 {
			break
		}
		end := idx + len(jdNow)
		for end < len(sql) && sql[end] != ')' {
			end++
		}
		if end < len(sql) {
			sql = sql[:idx] + "julianday(now())" + sql[end+1:]
			lower = strings.ToLower(sql)
			continue
		}
		break
	}
	return sql
}

func replaceStrftime(sql string) string {
	lower := strings.ToLower(sql)
	start := 0
	for {
		idx := strings.Index(lower[start:], "strftime(")
		if idx == -1 {
			break
		}
		idx += start
		// find closing ')'
		paren := idx + len("strftime(")
		depth := 1
		for paren < len(sql) && depth > 0 {
			switch sql[paren] {
			case '(':
				depth++
			case ')':
				depth--
			}
			paren++
		}
		if depth == 0 {
			inner := sql[idx+len("strftime(") : paren-1]
			repl := strings.ReplaceAll(inner, "'now'", "now()")
			sql = sql[:idx+len("strftime(")] + repl + sql[paren-1:]
			lower = strings.ToLower(sql)
			start = paren
			continue
		}
		break
	}
	return sql
}

func removeNotIndexed(sql string) string {
	lower := strings.ToLower(sql)
	for {
		idx := strings.Index(lower, notIdx)
		if idx == -1 {
			break
		}
		sql = sql[:idx] + sql[idx+len(notIdx):]
		lower = strings.ToLower(sql)
	}
	return sql
}

func replaceTotal(sql string) string {
	lower := strings.ToLower(sql)
	var out strings.Builder
	i := 0
	for i < len(sql) {
		idx := strings.Index(lower[i:], totalFn)
		if idx == -1 {
			out.WriteString(sql[i:])
			break
		}
		idx += i
		out.WriteString(sql[i:idx])
		if idx > 0 && isIdentChar(lower[idx-1]) {
			out.WriteString(sql[idx : idx+1])
			i = idx + 1
			continue
		}
		j := idx + len(totalFn)
		if j < len(sql) && isIdentChar(lower[j]) {
			out.WriteString(sql[idx:j])
			i = j
			continue
		}
		k := j
		for k < len(sql) && unicode.IsSpace(rune(sql[k])) {
			k++
		}
		if k < len(sql) && sql[k] == '(' {
			start := k + 1
			end := start
			for end < len(sql) && sql[end] != ')' && sql[end] != '(' {
				end++
			}
			if end < len(sql) && sql[end] == ')' {
				inner := strings.TrimSpace(sql[start:end])
				out.WriteString("coalesce(sum(" + inner + "),0)")
				i = end + 1
				continue
			}
		}
		out.WriteString(sql[idx:j])
		i = j
	}
	return out.String()
}

func replaceZeroBlob(sql string) string {
	lower := strings.ToLower(sql)
	var out strings.Builder
	i := 0
	for i < len(sql) {
		idx := strings.Index(lower[i:], "zeroblob")
		if idx == -1 {
			out.WriteString(sql[i:])
			break
		}
		idx += i
		out.WriteString(sql[i:idx])
		if idx > 0 && isIdentChar(lower[idx-1]) {
			out.WriteString(sql[idx : idx+1])
			i = idx + 1
			continue
		}
		j := idx + len("zeroblob")
		if j < len(sql) && isIdentChar(lower[j]) {
			out.WriteString(sql[idx:j])
			i = j
			continue
		}
		k := j
		for k < len(sql) && unicode.IsSpace(rune(sql[k])) {
			k++
		}
		if k < len(sql) && sql[k] == '(' {
			start := k + 1
			end := start
			for end < len(sql) && sql[end] != ')' {
				end++
			}
			if end < len(sql) {
				inner := strings.TrimSpace(sql[start:end])
				out.WriteString("repeat('\\x00', " + inner + ")")
				i = end + 1
				continue
			}
		}
		out.WriteString(sql[idx:j])
		i = j
	}
	return out.String()
}

func replaceHexRandomBlob(sql string) string {
	lower := strings.ToLower(sql)
	pattern := "hex(randomblob("
	for {
		idx := strings.Index(lower, pattern)
		if idx == -1 {
			break
		}
		// find closing ')'
		start := idx + len(pattern)
		end := start
		depth := 1
		for end < len(sql) && depth > 0 {
			switch sql[end] {
			case '(':
				depth++
			case ')':
				depth--
			}
			end++
		}
		if depth == 0 {
			inner := strings.TrimSpace(sql[start : end-1])
			repl := "hex(random_bytes(" + inner + "))"
			sql = sql[:idx] + repl + sql[end:]
			lower = strings.ToLower(sql)
			continue
		}
		break
	}
	return sql
}

func removeEmptyParens(sql string, names ...string) string {
	out := sql
	for _, n := range names {
		out = strings.ReplaceAll(out, n+"()", n)
	}
	return out
}

// Convert rewrites common SQLite syntax to DuckDB syntax.
func Convert(sql string) string {
	out := sql
	for _, c := range funcConversions {
		out = replaceFunc(out, c.in, c.out)
	}
	for _, c := range wordConversions {
		out = replaceWord(out, c.in, c.out)
	}
	out = replaceDateTime(out)
	out = replaceDate(out)
	out = replaceTime(out)
	out = replaceJulianday(out)
	out = replaceStrftime(out)
	out = removeNotIndexed(out)
	out = replaceTotal(out)
	out = replaceZeroBlob(out)
	out = replaceHexRandomBlob(out)
	out = removeEmptyParens(out, "current_date", "current_time")
	return out
}
