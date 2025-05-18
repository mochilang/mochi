package main

import (
	"fmt"
	"os"

	"github.com/alexflint/go-arg"

	"mochi/ast"
	"mochi/interpreter"
	"mochi/parser"
	"mochi/types"
)

var (
	version   = "dev"     // set via -ldflags "-X main.version=..."
	gitCommit = "unknown" // set via -ldflags "-X main.gitCommit=..."
	buildTime = "unknown" // set via -ldflags "-X main.buildTime=..."
)

type Args struct {
	File     string `arg:"positional" help:"Path to .mochi source file"`
	Run      bool   `arg:"-r,--run" help:"Interpret and execute the program"`
	PrintAST bool   `arg:"--ast" help:"Print the parsed AST in Lisp format"`
	Version  bool   `arg:"--version" help:"Show version and exit"`
}

func main() {
	var args Args
	argParser := arg.MustParse(&args)

	if len(os.Args) == 1 {
		argParser.WriteHelp(os.Stdout)
		os.Exit(0)
	}

	if args.Version {
		printVersion()
		os.Exit(0)
	}

	if args.File == "" {
		fmt.Fprintln(os.Stderr, "error: no input file provided")
		argParser.WriteHelp(os.Stderr)
		os.Exit(1)
	}

	prog, err := parser.Parse(args.File)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}

	// Type check
	typeEnv := types.NewEnv(nil)
	typeErrors := types.Check(prog, typeEnv)
	if len(typeErrors) > 0 {
		fmt.Fprintln(os.Stderr, "\n== TYPECHECK FAILED ==")
		for i, err := range typeErrors {
			fmt.Fprintf(os.Stderr, "  %2d. %v\n", i+1, err)
		}
		os.Exit(1)
	}

	if args.PrintAST {
		fmt.Println("\n== AST ==")
		tree := ast.FromProgram(prog)
		fmt.Println(tree.String())
	}

	if args.Run {
		fmt.Println("\n== RUNNING ==")
		interp := interpreter.New(prog, typeEnv)
		if err := interp.Run(); err != nil {
			fmt.Fprintf(os.Stderr, "runtime error: %v\n", err)
			os.Exit(1)
		}
	}
}

func printVersion() {
	fmt.Printf("Mochi v%s\n", version)
	fmt.Printf("Git commit: %s\n", gitCommit)
	fmt.Printf("Built at:   %s\n", buildTime)
}
