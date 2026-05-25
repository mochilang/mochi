package main

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"github.com/google/uuid"
	"io"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"sort"
	"strings"
	"time"

	"github.com/charmbracelet/fang"
	"github.com/fatih/color"
	_ "github.com/lib/pq"
	"github.com/spf13/cobra"

	"mochi/runtime/llm"
	_ "mochi/runtime/llm/provider/chutes"
	_ "mochi/runtime/llm/provider/cohere"
	_ "mochi/runtime/llm/provider/echo"

	denoffi "mochi/runtime/ffi/deno"
	goffi "mochi/runtime/ffi/go"
	ffiinfo "mochi/runtime/ffi/infer"
	python "mochi/runtime/ffi/python"

	"mochi/ast"
	"mochi/archived/interpreter"
	cbuild "mochi/compiler3/build/c"
	gobuild "mochi/compiler3/build/go"
	aotcbuild "mochi/transpiler3/c/build"
	"mochi/mcp"
	"mochi/parser"
	"mochi/repl"
	"mochi/runtime/mod"
	vm "mochi/runtime/vm"
	"mochi/tools/db"
	"mochi/tools/lint"
	"mochi/types"
)

var (
	version   = "dev"
	gitCommit = "unknown"
	buildTime = "unknown"
)

type CLI struct {
	Run        *RunCmd        `arg:"subcommand:run" help:"Run a Mochi source file"`
	Build      *BuildCmd      `arg:"subcommand:build" help:"Compile a Mochi source file to a target language"`
	Test       *TestCmd       `arg:"subcommand:test" help:"Run test blocks inside a Mochi source file"`
	Init       *InitCmd       `arg:"subcommand:init" help:"Initialize a new Mochi module"`
	Get        *GetCmd        `arg:"subcommand:get" help:"Download module dependencies"`
	Repl       *ReplCmd       `arg:"subcommand:repl" help:"Start an interactive REPL session"`
	LLM        *LLMCmd        `arg:"subcommand:llm" help:"Send a prompt to the default LLM"`
	Infer      *InferCmd      `arg:"subcommand:infer" help:"Infer externs from a package"`
	Serve      *ServeCmd      `arg:"subcommand:serve" help:"Start MCP server over stdio"`
	Cheatsheet *CheatsheetCmd `arg:"subcommand:cheatsheet" help:"Print language cheatsheet"`
	Version    bool           `arg:"--version" help:"Print version info and exit"`
}

// BuildCmd is the MEP-43 close-out CLI shell: `mochi build` drives the
// Mochi-to-IR frontend + compiler3/emit/{go,c} emitter to produce a
// target-language artifact from a Mochi source. The flags map one-to-
// one to gobuild.Options / cbuild.Options fields. --target=c is the
// MEP-42 Phase 4.0 single-binary path: it emits one .c file under
// --out, shells to the system cc, and writes a native executable.
type BuildCmd struct {
	File           string `arg:"positional,required" help:"Path to .mochi source file"`
	Target         string `arg:"--target" default:"go" help:"Target language (go|c|c-aot)"`
	Emit           string `arg:"--emit" default:"executable" help:"Emit shape (executable|go-library|c). --target=c-aot accepts executable|c."`
	Out            string `arg:"--out" help:"Output path. --target=go|c: output directory; --target=c-aot: binary path"`
	KeepEmit       bool   `arg:"--keep-emit" help:"Retain the emitted source file(s) after build"`
	Module         string `arg:"--module" help:"Module path baked into go.mod (required for --emit=go-library)"`
	PkgName        string `arg:"--package" help:"Package name override"`
	RuntimeReplace string `arg:"--runtime-replace" help:"Local replace directive for mochi runtime (library mode)"`
	CC             string `arg:"--cc" help:"C compiler to invoke (default: $MOCHI_CC or 'cc'); --target=c only"`
	BinaryPath     string `arg:"--binary" help:"Output binary path; --target=c only"`
	Static         bool   `arg:"--portable" help:"Statically link the binary (musl/glibc-static); --target=c and --target=c-aot"`
	Triple         string `arg:"--triple" help:"Target triple (e.g. x86_64-linux-musl, aarch64-macos, wasm32-wasi); --target=c only. When set, the driver defaults to 'zig cc' and passes -target=<triple>"`
	Profile        string `arg:"--profile" default:"release" help:"Build profile for --target=c-aot: 'release' (default) or 'debug' (adds -fsanitize=address,undefined)"`
}

type RunCmd struct {
	File     string `arg:"positional" help:"Path to .mochi source file"`
	Expr     string `arg:"-e,--expr" help:"Run an inline Mochi expression instead of a file"`
	PrintAST bool   `arg:"--ast" help:"Print parsed AST in Lisp format"`
	Debug    bool   `arg:"--debug" help:"Enable debug output"`
	Memoize  bool   `arg:"--memo" help:"Enable memoization of pure functions"`
	Fold     bool   `arg:"--aot" help:"Fold pure calls before execution"`
	PrintIR  bool   `arg:"--ir" help:"Print VM assembly and exit"`
}

type TestCmd struct {
	Path    string `arg:"positional,required" help:"File or directory to test"`
	Debug   bool   `arg:"--debug" help:"Enable debug output"`
	Memoize bool   `arg:"--memo" help:"Enable memoization of pure functions"`
	Fold    bool   `arg:"--aot" help:"Fold pure calls before execution"`
}

type InitCmd struct {
	Path string `arg:"positional" help:"Module path (default current directory)"`
}

type GetCmd struct{}

type LintCmd struct {
	Files []string `arg:"positional,required" help:"Mochi source files"`
}

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
	cFile  = color.New(color.FgHiBlue, color.Bold).SprintFunc()
	cTest  = color.New(color.FgYellow).SprintFunc()
	cOK    = color.New(color.FgGreen).SprintFunc()
	cFail  = color.New(color.FgRed).SprintFunc()
)

func main() {
	color.NoColor = false

	rootCmd := newRootCmd()
	if err := fang.Execute(
		context.Background(),
		rootCmd,
		fang.WithVersion(version),
		fang.WithCommit(gitCommit),
	); err != nil {
		os.Exit(1)
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

func printTestStart(name string) {
	fmt.Printf("   %s %-30s ...", cTest("test"), name)
}

func printTestPass(d time.Duration) {
	fmt.Printf(" %s (%s)\n", cOK("ok"), formatDuration(d))
}

func printTestFail(err error, d time.Duration) {
	fmt.Printf(" %s %v (%s)\n", cFail("fail"), err, formatDuration(d))
}

func formatDuration(d time.Duration) string {
	switch {
	case d < time.Microsecond:
		return fmt.Sprintf("%dns", d.Nanoseconds())
	case d < time.Millisecond:
		return fmt.Sprintf("%.1fµs", float64(d.Microseconds()))
	case d < time.Second:
		return fmt.Sprintf("%.1fms", float64(d.Milliseconds()))
	default:
		return fmt.Sprintf("%.2fs", d.Seconds())
	}
}

// runBuild dispatches the `mochi build` subcommand. It maps CLI flags
// onto gobuild.Options / cbuild.Options and runs the Mochi-to-IR
// frontend + the target's emitter + (for --target=c) the system cc.
// MEP-43 close-out for --target=go; MEP-42 Phase 4.0 for --target=c.
func runBuild(cmd *BuildCmd) error {
	switch cmd.Target {
	case "go":
		return runBuildGo(cmd)
	case "c":
		return runBuildC(cmd)
	case "c-aot":
		return runBuildCAOT(cmd)
	default:
		return fmt.Errorf("build: unsupported --target=%q (expected go|c|c-aot)", cmd.Target)
	}
}

func runBuildGo(cmd *BuildCmd) error {
	var mode gobuild.Mode
	switch cmd.Emit {
	case "", "executable":
		mode = gobuild.ModeExecutable
	case "go-library":
		mode = gobuild.ModeLibrary
		if cmd.Module == "" {
			return fmt.Errorf("build: --emit=go-library requires --module=<path>")
		}
	default:
		return fmt.Errorf("build: unknown --emit value %q (expected executable|go-library)", cmd.Emit)
	}
	outDir := cmd.Out
	if outDir == "" {
		tmp, err := os.MkdirTemp("", "mochi-build-")
		if err != nil {
			return fmt.Errorf("build: temp dir: %w", err)
		}
		outDir = tmp
	}
	opts := gobuild.Options{
		Mode:           mode,
		OutDir:         outDir,
		KeepEmit:       cmd.KeepEmit,
		ModulePath:     cmd.Module,
		PkgName:        cmd.PkgName,
		RuntimeReplace: cmd.RuntimeReplace,
	}
	r, err := gobuild.BuildSource(cmd.File, opts)
	if err != nil {
		return err
	}
	if mode == gobuild.ModeExecutable {
		fmt.Printf("emitted %s\n", r.EntryPoint)
	} else {
		for _, f := range r.Files {
			fmt.Printf("emitted %s\n", f)
		}
	}
	return nil
}

// runBuildCAOT is the MEP-45 entry. It maps --out PATH (binary path,
// not directory) and --emit=c|executable onto transpiler3/c/build.Driver
// and runs the parse -> type-check -> lower -> emit -> cc pipeline.
//
// --emit=c (Phase 1.1): keep the generated C source as <out>.c next to
// the binary. Default --emit=executable produces the binary only.
func runBuildCAOT(cmd *BuildCmd) error {
	if cmd.File == "" {
		return errors.New("build: --target=c-aot requires a source file argument")
	}
	if cmd.Out == "" {
		return errors.New("build: --target=c-aot requires --out PATH (binary output path)")
	}
	var keepEmit bool
	switch cmd.Emit {
	case "", "executable":
		keepEmit = false
	case "c":
		keepEmit = true
	default:
		return fmt.Errorf("build: --target=c-aot does not support --emit=%q (expected executable|c)", cmd.Emit)
	}
	d := &aotcbuild.Driver{
		CC:       cmd.CC,
		KeepEmit: keepEmit,
		Static:   cmd.Static,
	}
	if err := d.Build(cmd.File, cmd.Out, cmd.Triple, cmd.Profile); err != nil {
		return err
	}
	if d.CacheHit {
		fmt.Printf("cached %s\n", cmd.Out)
	} else {
		fmt.Printf("binary %s\n", cmd.Out)
	}
	if keepEmit && d.EmittedCPath != "" {
		fmt.Printf("source %s\n", d.EmittedCPath)
	}
	return nil
}

func runBuildC(cmd *BuildCmd) error {
	if cmd.Emit != "" && cmd.Emit != "executable" {
		return fmt.Errorf("build: --target=c does not support --emit=%q (Phase 4.0 ships executable only)", cmd.Emit)
	}
	outDir := cmd.Out
	if outDir == "" {
		tmp, err := os.MkdirTemp("", "mochi-build-c-")
		if err != nil {
			return fmt.Errorf("build: temp dir: %w", err)
		}
		outDir = tmp
	}
	opts := cbuild.Options{
		OutDir:     outDir,
		BinaryPath: cmd.BinaryPath,
		KeepEmit:   cmd.KeepEmit,
		CC:         cmd.CC,
		Static:     cmd.Static,
		Triple:     cmd.Triple,
	}
	r, err := cbuild.BuildSource(cmd.File, opts)
	if err != nil {
		return err
	}
	fmt.Printf("binary %s\n", r.BinaryPath)
	if cmd.KeepEmit {
		fmt.Printf("source %s\n", r.SourcePath)
	}
	return nil
}

func runFile(cmd *RunCmd) error {
	start := time.Now()
	if cmd.Expr != "" {
		dir, err := os.MkdirTemp("", "mochi-eval-")
		if err != nil {
			return fmt.Errorf("creating temp dir: %w", err)
		}
		defer os.RemoveAll(dir)
		path := filepath.Join(dir, "expr.mochi")
		if err := os.WriteFile(path, []byte(cmd.Expr+"\n"), 0o644); err != nil {
			return fmt.Errorf("writing expression: %w", err)
		}
		cmd.File = path
	}
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

	os.Setenv("MOCHI_ROOT", modRoot)
	p, errc := vm.Compile(prog, env)
	if errc != nil {
		return errc
	}
	if cmd.PrintIR {
		fmt.Print(p.Disassemble(string(source)))
		return nil
	}
	m := vm.New(p, os.Stdout)
	err = m.Run()
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
	path := cmd.Path
	if strings.HasSuffix(path, "/...") {
		return runTestsInDir(strings.TrimSuffix(path, "/..."), true, cmd)
	}
	info, err := os.Stat(path)
	if err != nil {
		return err
	}
	if info.IsDir() {
		return runTestsInDir(path, true, cmd)
	}
	return testFile(path, cmd)
}

func runTestsInDir(dir string, recursive bool, cmd *TestCmd) error {
	var files []string
	if recursive {
		filepath.WalkDir(dir, func(p string, d fs.DirEntry, err error) error {
			if err != nil {
				return err
			}
			if !d.IsDir() && strings.HasSuffix(d.Name(), ".mochi") {
				files = append(files, p)
			}
			return nil
		})
	} else {
		entries, err := os.ReadDir(dir)
		if err != nil {
			return err
		}
		for _, e := range entries {
			if !e.IsDir() && strings.HasSuffix(e.Name(), ".mochi") {
				files = append(files, filepath.Join(dir, e.Name()))
			}
		}
	}
	sort.Strings(files)
	var failures int
	for _, f := range files {
		if err := testFile(f, cmd); err != nil {
			failures++
		}
	}
	if failures > 0 {
		return fmt.Errorf("%d file(s) failed", failures)
	}
	return nil
}

func testFile(file string, cmd *TestCmd) error {
	rel := file
	if r, err := filepath.Rel(".", file); err == nil {
		rel = r
	}
	fmt.Println(cFile(rel))

	prog, err := parseOrPrintError(file)
	if err != nil {
		return err
	}
	env := types.NewEnv(nil)
	modRoot, errRoot := mod.FindRoot(filepath.Dir(file))
	if errRoot != nil {
		modRoot = filepath.Dir(file)
	}
	if errs := types.Check(prog, env); len(errs) > 0 {
		printTypeErrors(errs)
		return fmt.Errorf("aborted due to type errors")
	}
	if cmd.Fold {
		interpreter.FoldPureCalls(prog, env)
	}

	// Prepare a list of statements that should run before each test case.
	// Previously only function, let and type declarations were included
	// which meant that top-level side effecting statements (like `var`
	// declarations or loops) were skipped.  SQLLogicTest generated programs
	// rely on those statements to populate variables used inside test
	// blocks.  Include every statement except the tests themselves so that
	// the program state is fully initialised before each test runs.
	base := make([]*parser.Statement, 0, len(prog.Statements))
	for _, st := range prog.Statements {
		if st.Test == nil {
			base = append(base, st)
		}
	}

	var failures int
	for _, st := range prog.Statements {
		if st.Test == nil {
			continue
		}
		printTestStart(st.Test.Name)
		childEnv := types.NewEnv(env)
		start := time.Now()
		testProg := &parser.Program{Statements: append([]*parser.Statement(nil), base...)}
		testProg.Statements = append(testProg.Statements, st.Test.Body...)
		os.Setenv("MOCHI_ROOT", modRoot)
		p, errc := vm.Compile(testProg, childEnv)
		var err error
		if errc == nil {
			m := vm.New(p, io.Discard)
			err = m.Run()
		} else {
			err = errc
		}
		dur := time.Since(start)
		if err != nil {
			printTestFail(err, dur)
			failures++
		} else {
			printTestPass(dur)
		}
	}
	if failures > 0 {
		fmt.Fprintf(os.Stderr, "\n%s %d test(s) failed.\n", cFail("[FAIL]"), failures)
		return fmt.Errorf("test failed: %d test(s) failed", failures)
	}
	return nil
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

func runLint(cmd *LintCmd) error {
	exitCode := 0
	for _, file := range cmd.Files {
		errs := lint.File(file)
		if len(errs) > 0 {
			exitCode = 1
			fmt.Printf("%s\n", file)
			for _, e := range errs {
				fmt.Println("  ", e)
			}
		}
	}
	if exitCode != 0 {
		return fmt.Errorf("lint issues found")
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
		writeDoc(&b, t.Doc)
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
			writeDoc(&b, m.Doc)
			fmt.Fprintf(&b, "extern fun %s.%s(%s)%s\n", t.Name, m.Name, formatParams(m.Params), formatResults(m.Results))
		}
	}
	for _, c := range info.Consts {
		writeDoc(&b, c.Doc)
		fmt.Fprintf(&b, "extern let %s.%s: %s\n", alias, c.Name, normalizeType(c.Type))
	}
	for _, v := range info.Vars {
		writeDoc(&b, v.Doc)
		fmt.Fprintf(&b, "extern var %s.%s: %s\n", alias, v.Name, normalizeType(v.Type))
	}
	for _, f := range info.Functions {
		writeDoc(&b, f.Doc)
		fmt.Fprintf(&b, "extern fun %s.%s(%s)%s\n", alias, f.Name, formatParams(f.Params), formatResults(f.Results))
	}
	return b.String()
}

func writeDoc(b *strings.Builder, doc string) {
	doc = strings.TrimSpace(doc)
	if doc == "" {
		return
	}
	for _, line := range strings.Split(doc, "\n") {
		fmt.Fprintf(b, "/// %s\n", strings.TrimSpace(line))
	}
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

func newRootCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "mochi",
		Short: "Mochi command-line interface",
	}

	cmd.AddCommand(
		newRunCmd(),
		newBuildCmd(),
		newTestCmd(),
		newInitCmd(),
		newGetCmd(),
		newLintCmd(),
		newReplCmd(),
		newLLMCmd(),
		newInferCmd(),
		newServeCmd(),
		newCheatsheetCmd(),
	)
	return cmd
}

func newRunCmd() *cobra.Command {
	var rc RunCmd
	c := &cobra.Command{
		Use:   "run [file]",
		Short: "Run a Mochi source file or an inline expression with -e",
		Args:  cobra.MaximumNArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			if len(args) > 0 {
				rc.File = args[0]
			}
			if rc.Expr == "" && rc.File == "" {
				return fmt.Errorf("usage: mochi run <file> or mochi run -e '<expr>'")
			}
			if rc.Expr != "" && rc.File != "" {
				return fmt.Errorf("cannot combine -e/--expr with a file argument")
			}
			return runFile(&rc)
		},
	}
	c.Flags().StringVarP(&rc.Expr, "expr", "e", "", "Run an inline Mochi expression instead of a file")
	c.Flags().BoolVar(&rc.PrintAST, "ast", false, "Print parsed AST in Lisp format")
	c.Flags().BoolVar(&rc.Debug, "debug", false, "Enable debug output")
	c.Flags().BoolVar(&rc.Memoize, "memo", false, "Enable memoization of pure functions")
	c.Flags().BoolVar(&rc.Fold, "aot", false, "Fold pure calls before execution")
	c.Flags().BoolVar(&rc.PrintIR, "ir", false, "Print VM assembly and exit")
	return c
}

func newBuildCmd() *cobra.Command {
	var bc BuildCmd
	c := &cobra.Command{
		Use:   "build <file>",
		Short: "Compile a Mochi source file to a target language (MEP-43)",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			bc.File = args[0]
			return runBuild(&bc)
		},
	}
	c.Flags().StringVar(&bc.Target, "target", "go", "Target language (go|c|c-aot)")
	c.Flags().StringVar(&bc.Emit, "emit", "executable", "Emit shape (executable|go-library|c). --target=c-aot accepts executable|c (c keeps generated source as <out>.c).")
	c.Flags().StringVar(&bc.Out, "out", "", "Output path. --target=go: output directory; --target=c-aot: binary path; --target=c: output directory")
	c.Flags().BoolVar(&bc.KeepEmit, "keep-emit", false, "Retain the emitted source file(s) after build")
	c.Flags().StringVar(&bc.Module, "module", "", "Module path baked into go.mod (required for --emit=go-library)")
	c.Flags().StringVar(&bc.PkgName, "package", "", "Package name override")
	c.Flags().StringVar(&bc.RuntimeReplace, "runtime-replace", "", "Local replace directive for mochi runtime (library mode)")
	c.Flags().StringVar(&bc.CC, "cc", "", "C compiler to invoke (default: $MOCHI_CC or 'cc'); --target=c only")
	c.Flags().StringVar(&bc.BinaryPath, "binary", "", "Output binary path; --target=c only")
	c.Flags().BoolVar(&bc.Static, "portable", false, "Statically link the binary (musl/glibc-static); --target=c and --target=c-aot (Phase 17.3)")
	c.Flags().StringVar(&bc.Triple, "triple", "", "Target triple (e.g. x86_64-linux-musl, aarch64-macos, wasm32-wasi); --target=c only. When set, the driver defaults to 'zig cc' and passes -target=<triple>")
	return c
}

func newTestCmd() *cobra.Command {
	var tc TestCmd
	c := &cobra.Command{
		Use:   "test <path>",
		Short: "Run test blocks inside a Mochi source file",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			tc.Path = args[0]
			return runTests(&tc)
		},
	}
	c.Flags().BoolVar(&tc.Debug, "debug", false, "Enable debug output")
	c.Flags().BoolVar(&tc.Memoize, "memo", false, "Enable memoization of pure functions")
	c.Flags().BoolVar(&tc.Fold, "aot", false, "Fold pure calls before execution")
	return c
}

func newInitCmd() *cobra.Command {
	var ic InitCmd
	c := &cobra.Command{
		Use:   "init [path]",
		Short: "Initialize a new Mochi module",
		Args:  cobra.MaximumNArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			if len(args) > 0 {
				ic.Path = args[0]
			}
			return initModule(&ic)
		},
	}
	return c
}

func newGetCmd() *cobra.Command {
	c := &cobra.Command{
		Use:   "get",
		Short: "Download module dependencies",
		RunE: func(cmd *cobra.Command, args []string) error {
			return modGet(&GetCmd{})
		},
	}
	return c
}

func newLintCmd() *cobra.Command {
	var lc LintCmd
	c := &cobra.Command{
		Use:   "lint <file...>",
		Short: "Lint Mochi source files",
		Args:  cobra.MinimumNArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			lc.Files = args
			return runLint(&lc)
		},
	}
	return c
}

func newReplCmd() *cobra.Command {
	c := &cobra.Command{
		Use:   "repl",
		Short: "Start an interactive REPL session",
		Run: func(cmd *cobra.Command, args []string) {
			r := repl.New(os.Stdout, version)
			r.Run()
		},
	}
	return c
}

func newLLMCmd() *cobra.Command {
	var lc LLMCmd
	c := &cobra.Command{
		Use:   "llm [prompt]",
		Short: "Send a prompt to the default LLM",
		Args:  cobra.MaximumNArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			if len(args) > 0 {
				lc.Prompt = args[0]
			}
			return runLLM(&lc)
		},
	}
	c.Flags().StringVarP(&lc.File, "file", "f", "", "Read prompt from file ('-' for stdin)")
	return c
}

func newInferCmd() *cobra.Command {
	var ic InferCmd
	c := &cobra.Command{
		Use:   "infer <language> <package>",
		Short: "Infer externs from a package",
		Args:  cobra.ExactArgs(2),
		RunE: func(cmd *cobra.Command, args []string) error {
			ic.Language = args[0]
			ic.Package = args[1]
			return runInfer(&ic)
		},
	}
	c.Flags().StringVarP(&ic.Format, "format", "f", "mochi", "Output format (json|mochi)")
	return c
}

func newServeCmd() *cobra.Command {
	c := &cobra.Command{
		Use:   "serve",
		Short: "Start MCP server over stdio",
		RunE: func(cmd *cobra.Command, args []string) error {
			return mcp.ServeStdio()
		},
	}
	return c
}

func newCheatsheetCmd() *cobra.Command {
	c := &cobra.Command{
		Use:   "cheatsheet",
		Short: "Print language cheatsheet",
		Run: func(cmd *cobra.Command, args []string) {
			fmt.Print(mcp.Cheatsheet())
		},
	}
	return c
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
