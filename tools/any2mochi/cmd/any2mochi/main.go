package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"os"
	"strings"

	"mochi/tools/any2mochi"
)

func main() {
	server := flag.String("server", "", "language server command")
	lang := flag.String("lang", "", "language id")
	flag.Parse()

	if *server == "" || *lang == "" {
		fmt.Fprintln(os.Stderr, "usage: any2mochi -server <cmd> -lang <id> [file]")
		os.Exit(1)
	}

	var src string
	if flag.NArg() == 1 {
		data, err := os.ReadFile(flag.Arg(0))
		if err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
		src = string(data)
	} else {
		data, err := io.ReadAll(os.Stdin)
		if err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
		src = string(data)
	}

	parts := strings.Fields(*server)
	cmd := parts[0]
	args := parts[1:]
	syms, err := any2mochi.ParseText(cmd, args, *lang, src)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	enc.Encode(syms)
}
