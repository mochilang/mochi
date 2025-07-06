//go:build slow

package any2mochi

import (
	"path/filepath"
	"testing"

	"mochi/tools/any2mochi/x/ex"
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
	}{
		{"asm", "*.s.out", ConvertAsmFile, "asm"},
		{"c", "*.c.out", ConvertCFile, "c"},
		{"clj", "*.clj.out", ConvertCljFile, "clj"},
		{"cobol", "*.cob.out", ConvertCobolFile, "cobol"},
		{"cpp", "*.cpp.out", ConvertCppFile, "cpp"},
		{"cs", "*.cs.out", ConvertCsFile, "cs"},
		{"dart", "*.dart.out", ConvertDartFile, "dart"},
		{"erl", "*.erl.out", ConvertErlangFile, "erl"},
		{"ex", "*.ex.out", ex.FromLSPFile, "ex"},
		{"fortran", "*.f90.out", ConvertFortranFile, "fortran"},
		{"fs", "*.fs.out", ConvertFsFile, "fs"},
		{"hs", "*.hs.out", ConvertHsFile, "hs"},
		{"java", "*.java.out", ConvertJavaFile, "java"},
		{"kt", "*.kt.out", ConvertKtFile, "kt"},
		{"lua", "*.lua.out", ConvertLuaFile, "lua"},
		{"ocaml", "*.ml.out", ConvertOcamlFile, "ocaml"},
		{"pas", "*.pas.out", ConvertPasFile, "pas"},
		{"php", "*.php.out", ConvertPhpFile, "php"},
		{"pl", "*.pl.out", ConvertPrologFile, "prolog"},
		{"pl", "*.pl.out", ConvertPlFile, "pl"},
		{"rb", "*.rb.out", ConvertRbFile, "rb"},
		{"rkt", "*.rkt.out", ConvertRktFile, "rkt"},
		{"rust", "*.rs.out", ConvertRustFile, "rust"},
		{"scala", "*.scala.out", ConvertScalaFile, "scala"},
		{"scheme", "*.scm.out", ConvertSchemeFile, "scheme"},
		{"st", "*.st.out", ConvertStFile, "st"},
		{"swift", "*.swift.out", ConvertSwiftFile, "swift"},
		{"zig", "*.zig.out", ConvertZigFile, "zig"},
	}
	for _, l := range langs {
		dir := filepath.Join(root, "tests/compiler", l.dir)
		runConvertGolden(t, dir, l.pattern, l.convert, l.name, ".mochi", ".error")
	}
}
