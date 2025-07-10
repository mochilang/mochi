package testpkg

import (
	"strings"
	"testing"
)

func TestNineBillion(t *testing.T) {
	row := NineBillionRow(4)
	exp := []string{"1", "2", "1", "1"}
	for i, v := range row {
		if v != exp[i] {
			t.Fatalf("row[%d]=%s, expected %s", i, v, exp[i])
		}
	}
	if NineBillionSum(5) != "7" {
		t.Fatalf("sum(5) wrong: %s", NineBillionSum(5))
	}

	out := NineBillionExample()
	if !strings.Contains(out, "rows:") || !strings.Contains(out, "sums:") {
		t.Fatalf("example output missing sections")
	}
}
