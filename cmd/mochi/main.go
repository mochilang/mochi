package main

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"github.com/google/uuid"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"time"

	"github.com/alexflint/go-arg"
	"github.com/fatih/color"
	_ "github.com/lib/pq"

	"mochi/runtime/llm"
	_ "mochi/runtime/llm/provider/chutes"
	_ "mochi/runtime/llm/provider/cohere"
	_ "mochi/runtime/llm/provider/echo"

	denoffi "mochi/runtime/ffi/deno"
	goffi "mochi/runtime/ffi/go"
	ffiinfo "mochi/runtime/ffi/infer"
	python "mochi/runtime/ffi/python"

	"mochi/ast"
	"mochi/compile/go"
	"mochi/compile/py"
	"mochi/compile/ts"
	"mochi/compile/wasm"
	"mochi/interpreter"
	"mochi/mcp"
	"mochi/parser"
	"mochi/repl"
	"mochi/runtime/mod"
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
	Init       *InitCmd       `arg:"subcommand:init" help:"Initialize a new Mochi module"`
	Get        *GetCmd        `arg:"subcommand:get" help:"Download module dependencies"`
	Repl       *ReplCmd       `arg:"subcommand:repl" help:"Start an interactive REPL session"`
	LLM        *LLMCmd        `arg:"subcommand:llm" help:"Send a prompt to the default LLM"`
	Infer      *InferCmd      `arg:"subcommand:infer" help:"Infer externs from a package"`
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
	File          string `arg:"positional,required" help:"Path to .mochi source file"`
	Out           string `arg:"-o" help:"Output file path"`
	Target        string `arg:"--target" help:"Output language (go|py|ts|wasm)"`
	WasmToolchain string `arg:"--wasm-toolchain" help:"WASM toolchain (go|tinygo)"`
}

type InitCmd struct {
	Path string `arg:"positional" help:"Module path (default current directory)"`
}

type GetCmd struct{}

type ReplCmd struct{}
type LLMCmd struct {
	Prompt string `arg:"positional" help:"Prompt text"`
	File   string `arg:"-f" help:"Read prompt from file ('-' for stdin)"`
}
type InferCmd struct {
	Language string `arg:"positional,required" help:"Language (python|typescript|go)"`
	Package  string `arg:"positional,required" help:"Package or module path"`
	Format   string `arg:"-f,--format" default:"mochi" help:"Output format (json|mochi)"`
}
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
	case cli.LLM != nil:
		if err := runLLM(cli.LLM); err != nil {
			fmt.Fprintf(os.Stderr, "%s %v\n", cError("llm:"), err)
			os.Exit(1)
		}
	case cli.Infer != nil:
		if err := runInfer(cli.Infer); err != nil {
			fmt.Fprintf(os.Stderr, "%s %v\n", cError("infer:"), err)
			os.Exit(1)
		}
	case cli.Build != nil:
		if err := build(cli.Build); err != nil {
			fmt.Fprintf(os.Stderr, "%s %v\n", cError("build:"), err)
			os.Exit(1)
		}
	case cli.Init != nil:
		if err := initModule(cli.Init); err != nil {
			fmt.Fprintf(os.Stderr, "%s %v\n", cError("init:"), err)
			os.Exit(1)
		}
	case cli.Get != nil:
		if err := modGet(cli.Get); err != nil {
			fmt.Fprintf(os.Stderr, "%s %v\n", cError("get:"), err)
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
	modRoot, errRoot := mod.FindRoot(filepath.Dir(cmd.File))
	if errRoot != nil {
		modRoot = filepath.Dir(cmd.File)
	}
	if errs := types.Check(prog, env); len(errs) > 0 {
		printTypeErrors(errs)
		return fmt.Errorf("aborted due to type errors")
	}
	if cmd.Fold {
		interpreter.FoldPureCalls(prog, env)
	}
	memo := cmd.Memoize || os.Getenv("MOCHI_MEMO") == "1" || strings.ToLower(os.Getenv("MOCHI_MEMO")) == "true"
	interp := interpreter.New(prog, env, modRoot)
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
	modRoot, errRoot := mod.FindRoot(filepath.Dir(cmd.File))
	if errRoot != nil {
		modRoot = filepath.Dir(cmd.File)
	}
	if errs := types.Check(prog, env); len(errs) > 0 {
		printTypeErrors(errs)
		return fmt.Errorf("aborted due to type errors")
	}
	if cmd.Fold {
		interpreter.FoldPureCalls(prog, env)
	}
	memo := cmd.Memoize || os.Getenv("MOCHI_MEMO") == "1" || strings.ToLower(os.Getenv("MOCHI_MEMO")) == "true"
	interp := interpreter.New(prog, env, modRoot)
	interp.SetMemoization(memo)
	return interp.Test()
}

func runLLM(cmd *LLMCmd) error {
	var data []byte
	var err error
	switch {
	case cmd.File != "":
		if cmd.File == "-" {
			data, err = io.ReadAll(os.Stdin)
		} else {
			data, err = os.ReadFile(cmd.File)
		}
	case cmd.Prompt != "":
		data = []byte(cmd.Prompt)
	default:
		data, err = io.ReadAll(os.Stdin)
	}
	if err != nil {
		return err
	}
	prompt := string(data)
	stream, err := llm.ChatStream(context.Background(), []llm.Message{{Role: "user", Content: prompt}})
	if err != nil {
		return err
	}
	defer stream.Close()
	for {
		ch, err := stream.Recv()
		if err != nil {
			return err
		}
		if ch != nil {
			if ch.Content != "" {
				fmt.Print(ch.Content)
			}
			if ch.Done {
				break
			}
		}
	}
	fmt.Println()
	return nil
}

func runInfer(cmd *InferCmd) error {
	lang := strings.ToLower(cmd.Language)
	format := strings.ToLower(cmd.Format)
	if format == "" {
		format = "mochi"
	}

	var info *ffiinfo.ModuleInfo
	var err error
	switch lang {
	case "python", "py":
		info, err = python.Infer(cmd.Package)
	case "typescript", "ts":
		info, err = denoffi.Infer(cmd.Package)
	case "go":
		info, err = goffi.Infer(cmd.Package)
	default:
		return fmt.Errorf("unknown language: %s", cmd.Language)
	}
	if err != nil {
		return err
	}

	switch format {
	case "json":
		data, err := json.MarshalIndent(info, "", "  ")
		if err != nil {
			return err
		}
		fmt.Println(string(data))
	case "mochi":
		alias := parser.AliasFromPath(cmd.Package)
		fmt.Printf("import %s %q as %s\n", lang, cmd.Package, alias)
		fmt.Print(externsForAlias(info, alias))
	default:
		return fmt.Errorf("unknown format: %s", cmd.Format)
	}
	return nil
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

	modRoot, errRoot := mod.FindRoot(filepath.Dir(cmd.File))
	if errRoot != nil {
		modRoot = filepath.Dir(cmd.File)
	}

	target := strings.ToLower(cmd.Target)
	if target == "" && cmd.Out != "" {
		switch strings.ToLower(filepath.Ext(cmd.Out)) {
		case ".go":
			target = "go"
		case ".py":
			target = "py"
		case ".ts":
			target = "ts"
		case ".wasm":
			target = "wasm"
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
		code, err := tscode.New(env, modRoot).Compile(prog)
		if err == nil {
			err = os.WriteFile(out, code, 0644)
		}
		if err != nil {
			status = "error"
			msg = err.Error()
		}
	case "wasm":
		if out == "" {
			out = base + ".wasm"
		}
		tc := wasm.ToolchainGo
		if strings.ToLower(cmd.WasmToolchain) == "tinygo" {
			tc = wasm.ToolchainTinyGo
		} else if cmd.WasmToolchain != "" && strings.ToLower(cmd.WasmToolchain) != "go" {
			return fmt.Errorf("unknown wasm toolchain: %s", cmd.WasmToolchain)
		}
		data, err := wasm.New(env, wasm.WithToolchain(tc)).Compile(prog)
		if err == nil {
			err = os.WriteFile(out, data, 0644)
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

func initModule(cmd *InitCmd) error {
	modPath := cmd.Path
	if modPath == "" {
		cwd, err := os.Getwd()
		if err != nil {
			return err
		}
		modPath = filepath.Base(cwd)
	}
	if _, err := os.Stat("go.mod"); err == nil {
		return fmt.Errorf("go.mod already exists")
	}
	c := exec.Command("go", "mod", "init", modPath)
	c.Stdout = os.Stdout
	c.Stderr = os.Stderr
	return c.Run()
}

func modGet(cmd *GetCmd) error {
	c := exec.Command("go", "mod", "tidy")
	c.Stdout = os.Stdout
	c.Stderr = os.Stderr
	if err := c.Run(); err != nil {
		return err
	}
	c = exec.Command("go", "mod", "download")
	c.Stdout = os.Stdout
	c.Stderr = os.Stderr
	return c.Run()
}

func externsForAlias(info *ffiinfo.ModuleInfo, alias string) string {
	var b strings.Builder
	for _, t := range info.Types {
		if len(t.Fields) == 0 {
			fmt.Fprintf(&b, "extern type %s\n", t.Name)
		} else {
			fmt.Fprintf(&b, "extern type %s {\n", t.Name)
			for i, f := range t.Fields {
				fmt.Fprintf(&b, "  %s: %s", f.Name, f.Type)
				if i < len(t.Fields)-1 {
					b.WriteString(",\n")
				} else {
					b.WriteString("\n")
				}
			}
			b.WriteString("}\n")
		}
		for _, m := range t.Methods {
			fmt.Fprintf(&b, "extern fun %s.%s(%s)%s\n", t.Name, m.Name, formatParams(m.Params), formatResults(m.Results))
		}
	}
	for _, c := range info.Consts {
		fmt.Fprintf(&b, "extern let %s.%s: %s\n", alias, c.Name, normalizeType(c.Type))
	}
	for _, v := range info.Vars {
		fmt.Fprintf(&b, "extern var %s.%s: %s\n", alias, v.Name, normalizeType(v.Type))
	}
	for _, f := range info.Functions {
		fmt.Fprintf(&b, "extern fun %s.%s(%s)%s\n", alias, f.Name, formatParams(f.Params), formatResults(f.Results))
	}
	return b.String()
}

func formatParams(ps []ffiinfo.ParamInfo) string {
	if len(ps) == 0 {
		return ""
	}
	var b strings.Builder
	for i, p := range ps {
		if i > 0 {
			b.WriteString(", ")
		}
		if p.Name != "" {
			b.WriteString(p.Name)
			if p.Type != "" {
				b.WriteString(": ")
				b.WriteString(p.Type)
			}
		} else if p.Type != "" {
			b.WriteString(p.Type)
		}
	}
	return b.String()
}

func formatResults(rs []ffiinfo.ParamInfo) string {
	if len(rs) == 0 {
		return ""
	}
	if len(rs) == 1 {
		if rs[0].Type == "" {
			return ""
		}
		return ": " + rs[0].Type
	}
	var b strings.Builder
	b.WriteString(": (")
	for i, r := range rs {
		if i > 0 {
			b.WriteString(", ")
		}
		b.WriteString(r.Type)
	}
	b.WriteString(")")
	return b.String()
}

func normalizeType(t string) string {
	if strings.HasPrefix(t, "untyped ") {
		return strings.TrimPrefix(t, "untyped ")
	}
	return t
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
