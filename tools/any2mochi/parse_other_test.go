//go:build slow

package any2mochi

import (
	"testing"
)

var parseSnippets = map[string]string{
	"c":       "int main() { return 0; }",
	"cpp":     "int main() { return 0; }",
	"asm":     "main:\n  nop",
	"clj":     "(defn main [] nil)",
	"cobol":   "IDENTIFICATION DIVISION.\nPROGRAM-ID. HELLO.",
	"jvm":     "method public static main()V\n  return\n.end method",
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

func TestParseOtherLanguages(t *testing.T) {
	for lang, src := range parseSnippets {
		ls := Servers[lang]
		if err := EnsureServer(ls.Command); err != nil {
			t.Skipf("%s: %v", lang, err)
			continue
		}
		requireBinary(t, ls.Command)
		syms, diags, err := ParseText(ls.Command, ls.Args, ls.LangID, src)
		if err != nil {
			t.Errorf("%s parse error: %v", lang, err)
			continue
		}
		if len(diags) > 0 {
			t.Errorf("%s diagnostics: %v", lang, diags)
		}
		if len(syms) == 0 {
			t.Errorf("%s: expected symbols", lang)
		}
	}
}
