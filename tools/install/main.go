package main

import (
	"fmt"

	"mochi/compile/go"
	"mochi/compile/py"
	"mochi/compile/ts"
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
}
