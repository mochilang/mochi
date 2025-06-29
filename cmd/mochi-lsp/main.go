package main

import (
	_ "github.com/tliron/commonlog/simple"

	lsp "mochi/tools/lsp"
)

func main() {
	srv := lsp.NewServer()
	srv.RunStdio()
}
