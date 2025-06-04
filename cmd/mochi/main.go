package main

import (
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"time"

	"github.com/alexflint/go-arg"
	"github.com/fatih/color"

	"mochi/ast"
	gocode "mochi/compile/go"
	pycode "mochi/compile/py"
	tscode "mochi/compile/ts"
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
	Build   *BuildCmd `arg:"subcommand:build" help:"Compile a Mochi source file"`
	Repl    *ReplCmd  `arg:"subcommand:repl" help:"Start an interactive REPL session"`
	Serve   *ServeCmd `arg:"subcommand:serve" help:"Start MCP server over stdio"`
	Version bool      `arg:"--version" help:"Print version info and exit"`
}

type RunCmd struct {
	File     string `arg:"positional,required" help:"Path to .mochi source file"`
	PrintAST bool   `arg:"--ast" help:"Print parsed AST in Lisp format"`
	Debug    bool   `arg:"--debug" help:"Enable debug output"`
}

type TestCmd struct {
	File  string `arg:"positional,required" help:"Path to .mochi source file"`
	Debug bool   `arg:"--debug" help:"Enable debug output"`
}

type BuildCmd struct {
        File string `arg:"positional,required" help:"Path to .mochi source file"`
        Out  string `arg:"-o" help:"Output file path"`
       Target string `arg:"--target" help:"Output language (go|py|ts)"`
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

func build(cmd *BuildCmd) error {
       target := strings.ToLower(cmd.Target)
       if target == "" && cmd.Out != "" {
               switch strings.ToLower(filepath.Ext(cmd.Out)) {
               case ".go":
                       target = "go"
               case ".py":
                       target = "py"
               case ".ts":
                       target = "ts"
               }
       }
       switch target {
       case "go":
               return buildGo(cmd)
       case "py":
               return buildPy(cmd)
       case "ts":
               return buildTS(cmd)
       default:
               return buildBinary(cmd)
       }
}

func buildBinary(cmd *BuildCmd) error {
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
	c := gocode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		return err
	}
	dir, err := os.MkdirTemp("", "mochi-build-*")
	if err != nil {
		return err
	}
	defer os.RemoveAll(dir)
	goFile := filepath.Join(dir, "main.go")
	if err := os.WriteFile(goFile, code, 0644); err != nil {
		return err
	}
	out := cmd.Out
	if out == "" {
		base := strings.TrimSuffix(filepath.Base(cmd.File), filepath.Ext(cmd.File))
		if base == "" {
			base = "a.out"
		}
		out = base
	}
	bcmd := exec.Command("go", "build", "-o", out, goFile)
	bcmd.Env = append(os.Environ(), "GO111MODULE=off")
	bcmd.Stdout = os.Stdout
	bcmd.Stderr = os.Stderr
	if err := bcmd.Run(); err != nil {
		return fmt.Errorf("go build failed: %w", err)
	}
	return nil
}

func buildGo(cmd *BuildCmd) error {
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
	c := gocode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		return err
	}
	out := cmd.Out
	if out == "" {
		base := strings.TrimSuffix(filepath.Base(cmd.File), filepath.Ext(cmd.File))
		if base == "" {
			base = "main"
		}
		out = base + ".go"
	}
	if err := os.WriteFile(out, code, 0644); err != nil {
		return err
	}
	fmt.Printf("generated %s\n", out)
	return nil
}

func buildPy(cmd *BuildCmd) error {
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
	c := pycode.New()
	code, err := c.Compile(prog)
	if err != nil {
		return err
	}
	out := cmd.Out
	if out == "" {
		base := strings.TrimSuffix(filepath.Base(cmd.File), filepath.Ext(cmd.File))
		if base == "" {
			base = "main"
		}
		out = base + ".py"
	}
	if err := os.WriteFile(out, code, 0644); err != nil {
		return err
	}
	fmt.Printf("generated %s\n", out)
	return nil
}
func buildTS(cmd *BuildCmd) error {
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
	c := tscode.New()
	code, err := c.Compile(prog)
	if err != nil {
		return err
	}
	out := cmd.Out
	if out == "" {
		base := strings.TrimSuffix(filepath.Base(cmd.File), filepath.Ext(cmd.File))
		if base == "" {
			base = "a"
		}
		out = base + ".ts"
	}
	if err := os.WriteFile(out, code, 0644); err != nil {
		return err
	}
	fmt.Printf("generated %s\n", out)
	return nil
}
