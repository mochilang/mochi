package main

import (
	"context"
	"errors"
	"fmt"
	"github.com/google/uuid"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"time"

	"github.com/alexflint/go-arg"
	"github.com/fatih/color"
	_ "github.com/lib/pq"

	_ "mochi/runtime/llm/provider/chutes"
	_ "mochi/runtime/llm/provider/cohere"
	_ "mochi/runtime/llm/provider/echo"

	"mochi/ast"
	"mochi/compile/go"
	"mochi/compile/py"
	"mochi/compile/rs"
	"mochi/compile/ts"
	"mochi/interpreter"
	"mochi/mcp"
	"mochi/parser"
	"mochi/repl"
	"mochi/tools/db"
	"mochi/types"
)

var (
	version   = "dev"
	gitCommit = "unknown"
	buildTime = "unknown"
)

type CLI struct {
	Run        *RunCmd        `arg:"subcommand:run" help:"Run a Mochi source file"`
	Test       *TestCmd       `arg:"subcommand:test" help:"Run test blocks inside a Mochi source file"`
	Build      *BuildCmd      `arg:"subcommand:build" help:"Compile a Mochi source file"`
	Repl       *ReplCmd       `arg:"subcommand:repl" help:"Start an interactive REPL session"`
	Serve      *ServeCmd      `arg:"subcommand:serve" help:"Start MCP server over stdio"`
	Cheatsheet *CheatsheetCmd `arg:"subcommand:cheatsheet" help:"Print language cheatsheet"`
	Version    bool           `arg:"--version" help:"Print version info and exit"`
}

type RunCmd struct {
	File     string `arg:"positional,required" help:"Path to .mochi source file"`
	PrintAST bool   `arg:"--ast" help:"Print parsed AST in Lisp format"`
	Debug    bool   `arg:"--debug" help:"Enable debug output"`
	Memoize  bool   `arg:"--memo" help:"Enable memoization of pure functions"`
	Fold     bool   `arg:"--aot" help:"Fold pure calls before execution"`
}

type TestCmd struct {
	File    string `arg:"positional,required" help:"Path to .mochi source file"`
	Debug   bool   `arg:"--debug" help:"Enable debug output"`
	Memoize bool   `arg:"--memo" help:"Enable memoization of pure functions"`
	Fold    bool   `arg:"--aot" help:"Fold pure calls before execution"`
}

type BuildCmd struct {
	File   string `arg:"positional,required" help:"Path to .mochi source file"`
	Out    string `arg:"-o" help:"Output file path"`
	Target string `arg:"--target" help:"Output language (go|py|ts|rs)"`
}

type ReplCmd struct{}
type ServeCmd struct{}
type CheatsheetCmd struct{}

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
	case cli.Cheatsheet != nil:
		fmt.Print(mcp.Cheatsheet())
	case cli.Repl != nil:
		repl := repl.New(os.Stdout, version)
		repl.Run()
	case cli.Build != nil:
		if err := build(cli.Build); err != nil {
			fmt.Fprintf(os.Stderr, "%s %v\n", cError("build:"), err)
			os.Exit(1)
		}
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
	start := time.Now()
	prog, err := parseOrPrintError(cmd.File)
	if err != nil {
		return err
	}
	source, _ := os.ReadFile(cmd.File)

	if cmd.PrintAST {
		fmt.Println(ast.FromProgram(prog).String())
		return nil
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		printTypeErrors(errs)
		return fmt.Errorf("aborted due to type errors")
	}
	if cmd.Fold {
		interpreter.FoldPureCalls(prog, env)
	}
	memo := cmd.Memoize || os.Getenv("MOCHI_MEMO") == "1" || strings.ToLower(os.Getenv("MOCHI_MEMO")) == "true"
	interp := interpreter.New(prog, env)
	interp.SetMemoization(memo)
	err = interp.Run()
	status := "ok"
	msg := ""
	if err != nil {
		status = "error"
		msg = err.Error()
	}
	db.LogRun(context.Background(), &db.RunModel{
		File:      cmd.File,
		Source:    string(source),
		Status:    status,
		Error:     msg,
		Agent:     getAgent(),
		SessionID: getSessionID(),
		Duration:  time.Since(start),
	})
	return err
}

func runTests(cmd *TestCmd) error {
	prog, err := parseOrPrintError(cmd.File)
	if err != nil {
		return err
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		printTypeErrors(errs)
		return fmt.Errorf("aborted due to type errors")
	}
	if cmd.Fold {
		interpreter.FoldPureCalls(prog, env)
	}
	memo := cmd.Memoize || os.Getenv("MOCHI_MEMO") == "1" || strings.ToLower(os.Getenv("MOCHI_MEMO")) == "true"
	interp := interpreter.New(prog, env)
	interp.SetMemoization(memo)
	return interp.Test()
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

func build(cmd *BuildCmd) error {
	start := time.Now()
	source, _ := os.ReadFile(cmd.File)

	target := strings.ToLower(cmd.Target)
	if target == "" && cmd.Out != "" {
		switch strings.ToLower(filepath.Ext(cmd.Out)) {
		case ".go":
			target = "go"
		case ".py":
			target = "py"
		case ".ts":
			target = "ts"
		case ".rs":
			target = "rs"
		}
	}

	prog, err := parseOrPrintError(cmd.File)
	if err != nil {
		return err
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		printTypeErrors(errs)
		return fmt.Errorf("aborted due to type errors")
	}

	out := cmd.Out
	base := strings.TrimSuffix(filepath.Base(cmd.File), filepath.Ext(cmd.File))
	if base == "" {
		base = "main"
	}
	status := "ok"
	msg := ""

	switch target {
	case "go":
		if out == "" {
			out = base + ".go"
		}
		code, err := gocode.New(env).Compile(prog)
		if err == nil {
			err = os.WriteFile(out, code, 0644)
		}
		if err != nil {
			status = "error"
			msg = err.Error()
		}
	case "py":
		if out == "" {
			out = base + ".py"
		}
		code, err := pycode.New(env).Compile(prog)
		if err == nil {
			err = os.WriteFile(out, code, 0644)
		}
		if err != nil {
			status = "error"
			msg = err.Error()
		}
	case "ts":
		if out == "" {
			out = base + ".ts"
		}
		code, err := tscode.New(env).Compile(prog)
		if err == nil {
			err = os.WriteFile(out, code, 0644)
		}
		if err != nil {
			status = "error"
			msg = err.Error()
		}
	case "rs":
		if out == "" {
			out = base + ".rs"
		}
		code, err := rscode.New(env).Compile(prog)
		if err == nil {
			err = os.WriteFile(out, code, 0644)
		}
		if err != nil {
			status = "error"
			msg = err.Error()
		}
	default:
		tmpDir, err := os.MkdirTemp("", "mochi-build-*")
		if err != nil {
			return err
		}
		defer os.RemoveAll(tmpDir)
		goFile := filepath.Join(tmpDir, "main.go")
		code, err := gocode.New(env).Compile(prog)
		if err == nil {
			err = os.WriteFile(goFile, code, 0644)
		}
		if err == nil {
			if out == "" {
				out = base
			}
			cmd := exec.Command("go", "build", "-o", out, goFile)
			cmd.Env = append(os.Environ(), "GO111MODULE=off")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			err = cmd.Run()
		}
		if err != nil {
			status = "error"
			msg = err.Error()
		}
	}

	db.LogBuild(context.Background(), &db.BuildModel{
		File:      cmd.File,
		Source:    string(source),
		Out:       out,
		Target:    target,
		Status:    status,
		Error:     msg,
		Agent:     getAgent(),
		SessionID: getSessionID(),
		Duration:  time.Since(start),
	})

	if err != nil {
		return err
	}

	fmt.Printf("generated %s\n", out)
	return nil
}

func printTypeErrors(errs []error) {
	fmt.Fprintln(os.Stderr, cError("type error:"))
	for i, err := range errs {
		fmt.Fprintf(os.Stderr, "  %2d. %v\n", i+1, err)
	}
}

func getAgent() string {
	if agent := os.Getenv("MOCHI_AGENT"); agent != "" {
		return agent
	}
	return "cli"
}

func getSessionID() string {
	if sid := os.Getenv("MOCHI_SESSION"); sid != "" {
		return sid
	}
	return uuid.NewString()
}
