package sqlite2duck

import "testing"

func TestConvert(t *testing.T) {
	tests := []struct{ in, out string }{
		{"SELECT ifnull(a,0) FROM t;", "SELECT coalesce(a,0) FROM t;"},
		{"SELECT substr(name,1,2) FROM t;", "SELECT substring(name,1,2) FROM t;"},
		{"SELECT group_concat(x, ',') FROM t;", "SELECT string_agg(x, ',') FROM t;"},
		{"SELECT datetime('now');", "SELECT now();"},
		{"SELECT date('now');", "SELECT current_date;"},
		{"SELECT randomblob(4);", "SELECT random_bytes(4);"},
	}
	for _, tt := range tests {
		got := Convert(tt.in)
		if got != tt.out {
			t.Fatalf("convert %q => %q, want %q", tt.in, got, tt.out)
		}
	}
}
