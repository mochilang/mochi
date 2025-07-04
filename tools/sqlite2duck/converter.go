package sqlite2duck

import (
	"regexp"
	"strings"
)

var replacer = strings.NewReplacer(
	"ifnull(", "coalesce(",
	"IFNULL(", "COALESCE(",
	"substr(", "substring(",
	"SUBSTR(", "SUBSTRING(",
	"group_concat(", "string_agg(",
	"GROUP_CONCAT(", "STRING_AGG(",
	"randomblob(", "random_bytes(",
	"RANDOMBLOB(", "RANDOM_BYTES(",
)

var reDateTimeNow = regexp.MustCompile(`(?i)datetime\s*\(\s*'now'\s*\)`)
var reDateNow = regexp.MustCompile(`(?i)date\s*\(\s*'now'\s*\)`)

// Convert rewrites common SQLite syntax to DuckDB syntax.
func Convert(sql string) string {
	out := replacer.Replace(sql)
	out = reDateTimeNow.ReplaceAllString(out, "now()")
	out = reDateNow.ReplaceAllString(out, "current_date")
	return out
}
