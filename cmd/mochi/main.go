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
	"mochi/mcp"
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
	Run     *RunCmd   `arg:"subcommand:run" help:"Run a Mochi source file"`
	Test    *TestCmd  `arg:"subcommand:test" help:"Run test blocks inside a Mochi source file"`
	Repl    *ReplCmd  `arg:"subcommand:repl" help:"Start an interactive REPL session"`
	Serve   *ServeCmd `arg:"subcommand:serve" help:"Start MCP server over stdio"`
	Version bool      `arg:"--version" help:"Print version info and exit"`
}

type RunCmd struct {
	File     string `arg:"positional,required" help:"Path to .mochi source file"`
	PrintAST bool   `arg:"--ast" help:"Print parsed AST in Lisp format"`
}

type TestCmd struct {
	File string `arg:"positional,required" help:"Path to .mochi source file"`
}

type ReplCmd struct{}
type ServeCmd struct{}

var (
	cError = color.New(color.FgRed, color.Bold).SprintFunc()
	cTitle = color.New(color.FgCyan, color.Bold).SprintFunc()
)

func main() {
	var cli CLI
	arg.MustParse(&cli)
	color.NoColor = false

	switch {
	case cli.Version:
		printVersion()
	case cli.Repl != nil:
		repl := repl.New(os.Stdout, version)
		repl.Run()
	case cli.Run != nil:
		if err := runFile(cli.Run); err != nil {
			fmt.Fprintf(os.Stderr, "%s %v\n", cError("error:"), err)
			os.Exit(1)
		}
	case cli.Test != nil:
		if err := runTests(cli.Test); err != nil {
			fmt.Fprintf(os.Stderr, "%s %v\n", cError("test failed:"), err)
			os.Exit(1)
		}
	case cli.Serve != nil:
		if err := mcp.ServeStdio(); err != nil {
			fmt.Fprintf(os.Stderr, "%s %v\n", cError("mcp:"), err)
			os.Exit(1)
		}
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
	prog, err := parseOrPrintError(cmd.File)
	if err != nil {
		return err
	}
	if cmd.PrintAST {
		fmt.Println(ast.FromProgram(prog).String())
		return nil
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		fmt.Fprintln(os.Stderr, cError("type error:"))
		for i, err := range errs {
			fmt.Fprintf(os.Stderr, "  %2d. %v\n", i+1, err)
		}
		return fmt.Errorf("aborted due to type errors")
	}
	return interpreter.New(prog, env).Run()
}

func runTests(cmd *TestCmd) error {
	prog, err := parseOrPrintError(cmd.File)
	if err != nil {
		return err
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		fmt.Fprintln(os.Stderr, cError("type error:"))
		for i, err := range errs {
			fmt.Fprintf(os.Stderr, "  %2d. %v\n", i+1, err)
		}
		return fmt.Errorf("aborted due to type errors")
	}
	return interpreter.New(prog, env).Test()
}

func parseOrPrintError(path string) (*parser.Program, error) {
	prog, err := parser.Parse(path)
	if err != nil {
		var pathErr *os.PathError
		if errors.As(err, &pathErr) && os.IsNotExist(pathErr.Err) {
			return nil, fmt.Errorf("file not found: %s", path)
		}
		return nil, fmt.Errorf("parse error:\n  %v", err)
	}
	return prog, nil
}
