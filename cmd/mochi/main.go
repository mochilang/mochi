package main

import (
	"errors"
	"fmt"
	"os"
	"runtime"
	"time"

	"github.com/alexflint/go-arg"
	"github.com/fatih/color"

	"mochi/ast"
	"mochi/interpreter"
	"mochi/parser"
	"mochi/repl"
	"mochi/types"
)

var (
	version   = "dev"
	gitCommit = "unknown"
	buildTime = "unknown"
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

// Minimal, purposeful color
var (
	cError = color.New(color.FgRed, color.Bold).SprintFunc()
	cTitle = color.New(color.FgCyan).SprintFunc()
)

func main() {
	var cli CLI
	arg.MustParse(&cli)

	color.NoColor = false

	switch {
	case cli.Version:
		printVersion()
		return

	case cli.Repl != nil:
		repl := repl.New(os.Stdout, version)
		repl.Run()
		return

	case cli.Run != nil:
		if err := runFile(cli.Run); err != nil {
			fmt.Fprintf(os.Stderr, "%v\n", err)
			os.Exit(1)
		}
		return

	default:
		arg.MustParse(&cli).WriteHelp(os.Stderr)
		os.Exit(2)
	}
}

func printVersion() {
	fmt.Printf("%s v%s (%s, built %s on %s/%s)\n",
		cTitle("Mochi"),
		version,
		gitCommit,
		humanBuildTime(),
		runtime.GOOS,
		runtime.GOARCH,
	)
}

func humanBuildTime() string {
	t, err := time.Parse(time.RFC3339, buildTime)
	if err != nil {
		return buildTime
	}
	return t.Format("Mon Jan 02 15:04:05 2006")
}

func runFile(cmd *RunCmd) error {
	prog, err := parser.Parse(cmd.File)
	if err != nil {
		var pathErr *os.PathError
		if errors.As(err, &pathErr) && os.IsNotExist(pathErr.Err) {
			return fmt.Errorf("file not found: %s", cmd.File)
		}
		return fmt.Errorf("parse error:\n  %v", err)
	}

	if cmd.PrintAST {
		tree := ast.FromProgram(prog)
		fmt.Println(tree.String())
		return nil
	}

	env := types.NewEnv(nil)
	typeErrors := types.Check(prog, env)
	if len(typeErrors) > 0 {
		fmt.Fprintln(os.Stderr)
		fmt.Fprintln(os.Stderr, cError("type error:"))
		for i, err := range typeErrors {
			fmt.Fprintf(os.Stderr, "  %2d. %v\n", i+1, err)
		}
	}

	interp := interpreter.New(prog, env)
	if err := interp.Run(); err != nil {
		return fmt.Errorf("runtime error: %w", err)
	}

	return nil
}
