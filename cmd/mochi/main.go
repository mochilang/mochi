package main

import (
	"fmt"
	"os"
	"runtime"
	"time"

	"github.com/alexflint/go-arg"

	"mochi/ast"
	"mochi/interpreter"
	"mochi/parser"
	"mochi/repl"
	"mochi/types"
)

var (
	version   = "dev"     // set via -ldflags "-X main.version=..."
	gitCommit = "unknown" // set via -ldflags "-X main.gitCommit=..."
	buildTime = "unknown" // set via -ldflags "-X main.buildTime=..."
)

type CLI struct {
	Run     *RunCmd  `arg:"subcommand:run" help:"Run a Mochi source file"`
	Repl    *ReplCmd `arg:"subcommand:repl" help:"Start an interactive REPL session"`
	Version bool     `arg:"--version" help:"Print version info and exit"`
}

type RunCmd struct {
	File     string `arg:"positional,required" help:"Path to .mochi source file"`
	PrintAST bool   `arg:"--ast" help:"Print parsed AST in Lisp format"`
}

type ReplCmd struct{}

func main() {
	var cli CLI
	arg.MustParse(&cli)

	switch {
	case cli.Version:
		printVersion()
		return

	case cli.Repl != nil:
		repl := repl.New(os.Stdout, version)
		repl.Run()

	case cli.Run != nil:
		runFile(cli.Run)

	default:
		printVersion()
		arg.MustParse(&cli).WriteHelp(os.Stderr)
	}
}

func printVersion() {
	fmt.Printf("Mochi v%s (%s, built %s, %s/%s)\n", version, gitCommit, humanBuildTime(), runtime.GOOS, runtime.GOARCH)
}

func humanBuildTime() string {
	// Try parsing RFC 3339/ISO 8601 and format it to readable string
	t, err := time.Parse(time.RFC3339, buildTime)
	if err != nil {
		return buildTime // fallback: print raw string if invalid
	}
	return t.Format("Mon Jan 02 15:04:05 2006")
}

func runFile(cmd *RunCmd) {
	prog, err := parser.Parse(cmd.File)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}

	env := types.NewEnv(nil)
	typeErrors := types.Check(prog, env)
	if len(typeErrors) > 0 {
		fmt.Fprintln(os.Stderr, "\n== TYPECHECK FAILED ==")
		for i, err := range typeErrors {
			fmt.Fprintf(os.Stderr, "  %2d. %v\n", i+1, err)
		}
		os.Exit(1)
	}

	if cmd.PrintAST {
		fmt.Println("\n== AST ==")
		tree := ast.FromProgram(prog)
		fmt.Println(tree.String())
	}

	interp := interpreter.New(prog, env)
	if err := interp.Run(); err != nil {
		fmt.Fprintf(os.Stderr, "runtime error: %v\n", err)
		os.Exit(1)
	}
}
