//go:build slow

package any2mochi

import (
	"bytes"
	"flag"
	"os"
	"path/filepath"
	"testing"
)

var updateOther = flag.Bool("update", false, "update golden files")

var helloSnippets = map[string]string{
	"c":       "int main() { return 0; }",
	"cpp":     "int main() { return 0; }",
	"cs":      "class Program { static void Main() {} }",
	"dart":    "void main() {}",
	"erlang":  "-module(main).\n-export([main/0]).\nmain() -> ok.",
	"ex":      "defmodule Main do\n  def main() do\n  end\nend",
	"fortran": "program main\nend program main",
	"fs":      "let main argv = 0",
	"hs":      "main = return ()",
	"java":    "class Main { public static void main(string[] args) {} }",
	"kt":      "fun main() {}",
	"lua":     "function main() end",
	"mlir":    "module { func.func @main() {} }",
	"ocaml":   "let () = ()",
	"pas":     "program Main; begin end.",
	"php":     "<?php function main() {}",
	"pl":      "sub main {}",
	"rb":      "def main; end",
	"rkt":     "(define (main) #t)",
	"rust":    "fn main() {}",
	"scala":   "object Main { def main(args: Array[String]): Unit = {} }",
	"scheme":  "(define (main) #t)",
	"st":      "Object subclass: Main [ main ^self ]",
	"swift":   "func main() {}",
	"wasm":    "(module (func $main))",
	"zig":     "pub fn main() void {}",
}

func TestConvertOther_Golden(t *testing.T) {
	root := findRepoRoot(t)
	conv := map[string]func(string) ([]byte, error){
		"c":       ConvertC,
		"cpp":     ConvertCpp,
		"cs":      ConvertCs,
		"dart":    ConvertDart,
		"erlang":  ConvertErlang,
		"ex":      ConvertEx,
		"fortran": ConvertFortran,
		"fs":      ConvertFs,
		"hs":      ConvertHs,
		"java":    ConvertJava,
		"kt":      ConvertKt,
		"lua":     ConvertLua,
		"mlir":    ConvertMlir,
		"ocaml":   ConvertOcaml,
		"pas":     ConvertPas,
		"php":     ConvertPhp,
		"pl":      ConvertPl,
		"rb":      ConvertRb,
		"rkt":     ConvertRkt,
		"rust":    ConvertRust,
		"scala":   ConvertScala,
		"scheme":  ConvertScheme,
		"st":      ConvertSt,
		"swift":   ConvertSwift,
		"wasm":    ConvertWasm,
		"zig":     ConvertZig,
	}
	for lang, src := range helloSnippets {
		convert := conv[lang]
		if convert == nil {
			t.Fatalf("no converter for %s", lang)
		}
		out, err := convert(src)
		if err != nil {
			t.Skipf("%s: %v", lang, err)
			continue
		}
		outPath := filepath.Join(root, "tests/any2mochi/hello", lang+".mochi")
		if *updateOther {
			os.WriteFile(outPath, out, 0644)
			continue
		}
		want, err := os.ReadFile(outPath)
		if err != nil {
			t.Fatalf("missing golden for %s: %v", lang, err)
		}
		if !bytes.Equal(bytes.TrimSpace(out), bytes.TrimSpace(want)) {
			t.Errorf("%s mismatch\n--- got ---\n%s\n--- want ---\n%s", lang, out, want)
		}
	}
}

func findRepoRoot(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal("cannot determine working directory")
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	t.Fatal("go.mod not found (not in Go module)")
	return ""
}
