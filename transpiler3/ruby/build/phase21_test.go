package build

import (
	"path/filepath"
	"testing"
)

// TestPhase21StreamSaveCSV exercises subscribe_limit (drop-on-full
// subscriber) and saveCSV (CSV file writer).
func TestPhase21StreamSaveCSV(t *testing.T) {
	tc, err := resolveToolchain()
	if err != nil {
		t.Skipf("ruby toolchain not available: %v", err)
	}
	repoRoot := repoRootForTest(t)
	runtimeLib := filepath.Join(repoRoot, "mochi-runtime", "lib")

	t.Run("subscribe_limit_drops_overflow", func(t *testing.T) {
		src := "let s: stream<int> = make_stream(8)\n" +
			"let sub = subscribe_limit(s, 2)\n" +
			"emit(s, 1)\n" +
			"emit(s, 2)\n" +
			"emit(s, 3)\n" +
			"let a = recv_sub(sub)\n" +
			"let b = recv_sub(sub)\n" +
			"print(a)\n" +
			"print(b)\n"
		runRubyFixture(t, tc, runtimeLib, "subscribe_limit_drops_overflow", src, "1\n2\n")
	})

	t.Run("save_csv", func(t *testing.T) {
		outDir := t.TempDir()
		csvPath := filepath.Join(outDir, "out.csv")
		src := "let rows: list<list<string>> = [[\"a\", \"b\"], [\"c\", \"d\"]]\n" +
			"saveCSV(\"" + csvPath + "\", rows)\n" +
			"let back = loadCSV(\"" + csvPath + "\")\n" +
			"print(back[0][0])\n" +
			"print(back[1][1])\n"
		runRubyFixture(t, tc, runtimeLib, "save_csv", src, "a\nd\n")
	})
}
