//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"
)

// TestConvertOther_Golden converts sample code for a variety of languages
// using their language servers. The generated Mochi or any error message is
// compared against the golden files under tests/any2mochi/<lang>.
func TestConvertOther_Golden(t *testing.T) {
	root := findRepoRoot(t)
	langs := []struct {
		dir     string
		pattern string
		convert func(string) ([]byte, error)
		name    string
		extOut  string
		extErr  string
	}{
		{"c", "*.c.out", ConvertCFile, "c", ".c.mochi", ".c.error"},
		{"cpp", "*.cpp.out", ConvertCppFile, "cpp", ".cpp.mochi", ".cpp.error"},
		{"asm", "*.s.out", ConvertAsmFile, "asm", ".s.mochi", ".s.error"},
		{"clj", "*.clj.out", ConvertCljFile, "clj", ".clj.mochi", ".clj.error"},
		{"cobol", "*.cob.out", ConvertCobolFile, "cobol", ".cob.mochi", ".cob.error"},
		{"cs", "*.cs.out", ConvertCsFile, "cs", ".cs.mochi", ".cs.error"},
		{"hs", "*.hs.out", ConvertHsFile, "hs", ".hs.mochi", ".hs.error"},
		{"kt", "*.kt.out", ConvertKtFile, "kt", ".kt.mochi", ".kt.error"},
		{"lua", "*.lua.out", ConvertLuaFile, "lua", ".lua.mochi", ".lua.error"},
		{"rb", "*.rb.out", ConvertRbFile, "rb", ".rb.mochi", ".rb.error"},
		{"rust", "*.rs.out", ConvertRustFile, "rust", ".rs.mochi", ".rs.error"},
	}
	for _, l := range langs {
		dir := filepath.Join(root, "tests/compiler", l.dir)
		runConvertGolden(t, dir, l.pattern, l.convert, l.name, l.extOut, l.extErr)
	}
}
