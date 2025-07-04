package sqlite2duck

import (
	"regexp"
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

var reDateTimeNow = regexp.MustCompile(`(?i)datetime\s*\(\s*'now'\s*\)`)
var reDateNow = regexp.MustCompile(`(?i)date\s*\(\s*'now'\s*\)`)

// Convert rewrites common SQLite syntax to DuckDB syntax.
func Convert(sql string) string {
	out := sql
	for _, c := range conversions {
		out = c.re.ReplaceAllString(out, c.repl)
	}
	out = reDateTimeNow.ReplaceAllString(out, "now()")
	out = reDateNow.ReplaceAllString(out, "current_date")
	return out
}
