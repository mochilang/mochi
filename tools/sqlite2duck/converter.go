package sqlite2duck

import (
	"regexp"
	"strings"
)

var conversions = []struct {
	re   *regexp.Regexp
	repl string
}{
	{regexp.MustCompile(`(?i)\bifnull\s*\(`), "coalesce("},
	{regexp.MustCompile(`(?i)\bsubstr\s*\(`), "substring("},
	{regexp.MustCompile(`(?i)\bgroup_concat\s*\(`), "string_agg("},
	{regexp.MustCompile(`(?i)\brandomblob\s*\(`), "random_bytes("},
	{regexp.MustCompile(`(?i)\bcurrent_timestamp\b`), "now()"},
}

var (
	reDateTimeNow = regexp.MustCompile(`(?i)datetime\s*\(\s*'now'\s*\)`)
	reDateNow     = regexp.MustCompile(`(?i)date\s*\(\s*'now'\s*\)`)
	reTotal       = regexp.MustCompile(`(?i)total\s*\([^()]*\)`)
	reNotIndexed  = regexp.MustCompile(`(?i)\s+NOT\s+INDEXED\b`)
)

// Convert rewrites common SQLite syntax to DuckDB syntax.
func Convert(sql string) string {
	out := sql
	for _, c := range conversions {
		out = c.re.ReplaceAllString(out, c.repl)
	}
	out = reDateTimeNow.ReplaceAllString(out, "now()")
	out = reDateNow.ReplaceAllString(out, "current_date")
	out = reNotIndexed.ReplaceAllString(out, "")
	out = reTotal.ReplaceAllStringFunc(out, func(m string) string {
		inner := strings.TrimSpace(m[len("total(") : len(m)-1])
		return "coalesce(sum(" + inner + "),0)"
	})
	return out
}
