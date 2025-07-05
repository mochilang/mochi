//go:build slow

package sqlite2duck

import "testing"

func TestConvert(t *testing.T) {
	tests := []struct{ in, out string }{
		{"SELECT ifnull(a,0) FROM t;", "SELECT coalesce(a,0) FROM t;"},
		{"SELECT substr(name,1,2) FROM t;", "SELECT substring(name,1,2) FROM t;"},
		{"SELECT group_concat(x, ',') FROM t;", "SELECT string_agg(x, ',') FROM t;"},
		{"SELECT datetime('now');", "SELECT now();"},
		{"SELECT datetime('now','localtime');", "SELECT now();"},
		{"SELECT datetime('now','utc');", "SELECT now();"},
		{"SELECT date('now');", "SELECT current_date;"},
		{"SELECT date('now','localtime');", "SELECT current_date;"},
		{"SELECT date('now','utc');", "SELECT current_date;"},
		{"SELECT time('now');", "SELECT current_time;"},
		{"SELECT randomblob(4);", "SELECT random_bytes(4);"},
		{"SELECT IFNULL ( a , 0 ) FROM t;", "SELECT coalesce( a , 0 ) FROM t;"},
		{"SELECT CURRENT_TIMESTAMP;", "SELECT now();"},
		{"SELECT total(x) FROM t;", "SELECT coalesce(sum(x),0) FROM t;"},
		{"SELECT * FROM t NOT INDEXED;", "SELECT * FROM t;"},
		{"SELECT char_length(name) FROM t;", "SELECT length(name) FROM t;"},
		{"SELECT printf('%d', a) FROM t;", "SELECT format('%d', a) FROM t;"},
		{"SELECT instr(a, b) FROM t;", "SELECT strpos(a, b) FROM t;"},
		{"SELECT CURRENT_DATE;", "SELECT current_date;"},
		{"SELECT CURRENT_TIME;", "SELECT current_time;"},
		{"SELECT character_length(name) FROM t;", "SELECT length(name) FROM t;"},
		{"SELECT CURRENT_TIMESTAMP();", "SELECT now();"},
		{"SELECT CURRENT_DATE();", "SELECT current_date;"},
		{"SELECT CURRENT_TIME();", "SELECT current_time;"},
		{"SELECT julianday('now');", "SELECT julianday(now());"},
		{"SELECT strftime('%Y', 'now');", "SELECT strftime('%Y', now());"},
		{"SELECT zeroblob(2);", "SELECT repeat('\\x00', 2);"},
	}
	for _, tt := range tests {
		got := Convert(tt.in)
		if got != tt.out {
			t.Fatalf("convert %q => %q, want %q", tt.in, got, tt.out)
		}
	}
}
