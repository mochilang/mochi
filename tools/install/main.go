package main

import (
	"fmt"

	"mochi/compile/go"
	"mochi/compile/py"
	"mochi/compile/ts"
	"mochi/tools/any2mochi"
)

func main() {
	if err := tscode.EnsureDeno(); err != nil {
		fmt.Println("failed to install Deno:", err)
	}
	if err := gocode.EnsureGopls(); err != nil {
		fmt.Println("failed to install gopls:", err)
	}
	if err := pycode.EnsurePyright(); err != nil {
		fmt.Println("failed to install pyright:", err)
	}
	if err := tscode.EnsureTSLanguageServer(); err != nil {
		fmt.Println("failed to install typescript-language-server:", err)
	}
	for lang, ls := range any2mochi.Servers {
		if ls.Command == "" {
			continue
		}
		if err := any2mochi.EnsureServer(ls.Command); err != nil {
			fmt.Printf("failed to install %s server: %v\n", lang, err)
		}
	}
}
