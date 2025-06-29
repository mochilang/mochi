package main

import "mochi/tools/lsp"

func main() {
	s := lsp.New()
	_ = s.RunStdio()
}
