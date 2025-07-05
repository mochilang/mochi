package main

import (
	"flag"
	"fmt"
	"os"
	"strings"

	"mochi/tools/any2mochi"
)

func main() {
	server := flag.String("server", "", "LSP server command (e.g. 'pyright-langserver --stdio')")
	lang := flag.String("lang", "", "Language ID (e.g. python, typescript)")
	flag.Parse()
	if *server == "" || *lang == "" || flag.NArg() != 1 {
		fmt.Fprintln(os.Stderr, "usage: any2mochi -server <cmd> -lang <id> <file>")
		os.Exit(1)
	}
	cmdArgs := strings.Split(*server, " ")
	src, err := os.ReadFile(flag.Arg(0))
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	if err := any2mochi.RunLanguageServer(cmdArgs, *lang, string(src)); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}
