//go:build slow

package main

import (
	"bytes"
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

	goffi "mochi/runtime/ffi/go"
	ffiinfo "mochi/runtime/ffi/infer"
	python "mochi/runtime/ffi/python"

	ctrans "mochi/transpiler/x/c"
	cljt "mochi/transpiler/x/clj"
	cobol "mochi/transpiler/x/cobol"
	cpp "mochi/transpiler/x/cpp"
	cstranspiler "mochi/transpiler/x/cs"
	dartt "mochi/transpiler/x/dart"
	erl "mochi/transpiler/x/erl"
	ex "mochi/transpiler/x/ex"
	fortran "mochi/transpiler/x/fortran"
	fstrans "mochi/transpiler/x/fs"
	gotrans "mochi/transpiler/x/go"
	hs "mochi/transpiler/x/hs"
	javatr "mochi/transpiler/x/java"
	kt "mochi/transpiler/x/kt"
	lua "mochi/transpiler/x/lua"
	ocaml "mochi/transpiler/x/ocaml"
	pas "mochi/transpiler/x/pas"
	php "mochi/transpiler/x/php"
	pl "mochi/transpiler/x/pl"
	py "mochi/transpiler/x/py"
	rb "mochi/transpiler/x/rb"
	rkt "mochi/transpiler/x/rkt"
	rs "mochi/transpiler/x/rs"
	scalat "mochi/transpiler/x/scala"
	scheme "mochi/transpiler/x/scheme"
	st "mochi/transpiler/x/st"
	swifttrans "mochi/transpiler/x/swift"
	tstranspiler "mochi/transpiler/x/ts"
	zigt "mochi/transpiler/x/zig"

	cast "mochi/aster/x/c"
	cljast "mochi/aster/x/clj"
	cppast "mochi/aster/x/cpp"
	csast "mochi/aster/x/cs"
	dartast "mochi/aster/x/dart"
	elixirast "mochi/aster/x/elixir"
	fsast "mochi/aster/x/fs"
	goast "mochi/aster/x/go"
	haskellast "mochi/aster/x/haskell"
	hsast "mochi/aster/x/hs"
	javaast "mochi/aster/x/java"
	kotlinast "mochi/aster/x/kotlin"
	luaast "mochi/aster/x/lua"
	mochiins "mochi/aster/x/mochi"
	ocamlast "mochi/aster/x/ocaml"
	pasast "mochi/aster/x/pas"
	phpast "mochi/aster/x/php"
	plast "mochi/aster/x/pl"
	prologast "mochi/aster/x/prolog"
	pyast "mochi/aster/x/py"
	rbast "mochi/aster/x/rb"
	rsast "mochi/aster/x/rs"
	scalaast "mochi/aster/x/scala"
	schemeast "mochi/aster/x/scheme"
	swiftast "mochi/aster/x/swift"
	tsast "mochi/aster/x/ts"
	zigast "mochi/aster/x/zig"

	"mochi/ast"
	"mochi/interpreter"
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
	Test       *TestCmd       `arg:"subcommand:test" help:"Run test blocks inside a Mochi source file"`
	Init       *InitCmd       `arg:"subcommand:init" help:"Initialize a new Mochi module"`
	Get        *GetCmd        `arg:"subcommand:get" help:"Download module dependencies"`
	Repl       *ReplCmd       `arg:"subcommand:repl" help:"Start an interactive REPL session"`
	Build      *BuildCmd      `arg:"subcommand:build" help:"Compile to Go, TypeScript or Python"`
	BuildX     *BuildXCmd     `arg:"subcommand:buildx" help:"Compile to other languages"`
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
	PrintIR  bool   `arg:"--ir" help:"Print VM assembly and exit"`
}

type TestCmd struct {
	Path    string `arg:"positional,required" help:"File or directory to test"`
	Debug   bool   `arg:"--debug" help:"Enable debug output"`
	Memoize bool   `arg:"--memo" help:"Enable memoization of pure functions"`
	Fold    bool   `arg:"--aot" help:"Fold pure calls before execution"`
}

type BuildCmd struct {
	File   string `arg:"positional,required" help:"Source file"`
	Target string `arg:"-t,--target" help:"Target language"`
	Out    string `arg:"-o" help:"Output file (default stdout)"`
}

type BuildXCmd struct {
	File   string `arg:"positional,required" help:"Source file"`
	Target string `arg:"-t,--target" help:"Target language"`
	Out    string `arg:"-o" help:"Output file (default stdout)"`
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
	Language string `arg:"positional,required" help:"Language (python|go)"`
	Package  string `arg:"positional,required" help:"Package or module path"`
	Format   string `arg:"-f,--format" default:"mochi" help:"Output format (json|mochi)"`
}
type InspectCmd struct {
	Language string `arg:"positional,required" help:"Programming language"`
	File     string `arg:"positional,required" help:"Source file ('-' for stdin)"`
}
type PrintCmd struct {
	Language string `arg:"positional,required" help:"Programming language"`
	File     string `arg:"positional,required" help:"Source file ('-' for stdin)"`
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

func runInspect(cmd *InspectCmd) error {
	var data []byte
	var err error
	if cmd.File == "-" {
		data, err = io.ReadAll(os.Stdin)
	} else {
		data, err = os.ReadFile(cmd.File)
	}
	if err != nil {
		return err
	}
	src := string(data)
	lang := strings.ToLower(cmd.Language)
	var prog any
	switch lang {
	case "c":
		prog, err = cast.Inspect(src)
	case "clj", "clojure":
		prog, err = cljast.Inspect(src)
	case "cpp", "c++":
		prog, err = cppast.Inspect(src)
	case "cs", "csharp":
		prog, err = csast.Inspect(src)
	case "dart":
		prog, err = dartast.Inspect(src)
	case "elixir", "ex":
		prog, err = elixirast.Inspect(src)
	case "fs", "fsharp":
		prog, err = fsast.Inspect(src)
	case "go":
		prog, err = goast.Inspect(src)
	case "haskell":
		prog, err = haskellast.Inspect(src)
	case "hs":
		prog, err = hsast.Inspect(src)
	case "java":
		prog, err = javaast.Inspect(src, javaast.Options{})
	case "kotlin", "kt":
		prog, err = kotlinast.Inspect(src)
	case "lua":
		prog, err = luaast.Inspect(src)
	case "mochi":
		prog, err = mochiins.Inspect(src)
	case "ocaml":
		prog, err = ocamlast.Inspect(src)
	case "pas", "pascal":
		prog, err = pasast.Inspect(src)
	case "php":
		prog, err = phpast.Inspect(src, nil)
	case "pl":
		prog, err = plast.Inspect(src)
	case "prolog":
		prog, err = prologast.Inspect(src)
	case "py", "python":
		prog, err = pyast.Inspect(src)
	case "rb", "ruby":
		prog, err = rbast.Inspect(src)
	case "zig":
		prog, err = zigast.Inspect(src)
	case "rs", "rust":
		prog, err = rsast.Inspect(src, rsast.Option{})
	case "scala":
		prog, err = scalaast.Inspect(src)
	case "scheme":
		prog, err = schemeast.Inspect(src)
	case "swift":
		prog, err = swiftast.Inspect(src)
	case "ts", "typescript":
		prog, err = tsast.Inspect(src)
	default:
		return fmt.Errorf("unknown language: %s", cmd.Language)
	}
	if err != nil {
		return err
	}
	out, err := json.MarshalIndent(prog, "", "  ")
	if err != nil {
		return err
	}
	fmt.Println(string(out))
	return nil
}

func runPrint(cmd *PrintCmd) error {
	var data []byte
	var err error
	if cmd.File == "-" {
		data, err = io.ReadAll(os.Stdin)
	} else {
		data, err = os.ReadFile(cmd.File)
	}
	if err != nil {
		return err
	}
	src := string(data)
	lang := strings.ToLower(cmd.Language)
	var out string
	switch lang {
	case "c":
		prog, err := cast.Inspect(src)
		if err == nil {
			out, err = cast.Print(prog)
		}
		if err != nil {
			return err
		}
	case "clj", "clojure":
		prog, err := cljast.Inspect(src)
		if err == nil {
			out, err = cljast.Print(prog)
		}
		if err != nil {
			return err
		}
	case "cpp", "c++":
		prog, err := cppast.Inspect(src)
		if err == nil {
			out, err = cppast.Print(prog)
		}
		if err != nil {
			return err
		}
	case "cs", "csharp":
		prog, err := csast.Inspect(src)
		if err == nil {
			out, err = csast.Print(prog)
		}
		if err != nil {
			return err
		}
	case "dart":
		prog, err := dartast.Inspect(src)
		if err == nil {
			out, err = dartast.Print(prog)
		}
		if err != nil {
			return err
		}
	case "elixir", "ex":
		prog, err := elixirast.Inspect(src)
		if err == nil {
			out, err = elixirast.Print(prog)
		}
		if err != nil {
			return err
		}
	case "fs", "fsharp":
		prog, err := fsast.Inspect(src)
		if err == nil {
			out, err = fsast.Print(prog)
		}
		if err != nil {
			return err
		}
	case "go":
		prog, err := goast.Inspect(src)
		if err == nil {
			out, err = goast.Print(prog)
		}
		if err != nil {
			return err
		}
	case "haskell":
		prog, err := haskellast.Inspect(src)
		if err == nil {
			out, err = haskellast.Print(prog)
		}
		if err != nil {
			return err
		}
	case "java":
		prog, err := javaast.Inspect(src, javaast.Options{})
		if err == nil {
			out, err = javaast.Print(prog)
		}
		if err != nil {
			return err
		}
	case "kotlin", "kt":
		prog, err := kotlinast.Inspect(src)
		if err == nil {
			out, err = kotlinast.Print(prog)
		}
		if err != nil {
			return err
		}
	case "lua":
		prog, err := luaast.Inspect(src)
		if err == nil {
			out, err = luaast.Print(prog)
		}
		if err != nil {
			return err
		}
	case "mochi":
		prog, err := mochiins.Inspect(src)
		if err == nil {
			out, err = mochiins.Print(prog)
		}
		if err != nil {
			return err
		}
	case "ocaml":
		prog, err := ocamlast.Inspect(src)
		if err == nil {
			out, err = ocamlast.Print(prog)
		}
		if err != nil {
			return err
		}
	case "php":
		prog, err := phpast.Inspect(src, nil)
		if err == nil {
			out, err = phpast.Print(prog)
		}
		if err != nil {
			return err
		}
	case "prolog":
		prog, err := prologast.Inspect(src)
		if err == nil {
			out, err = prologast.Print(prog)
		}
		if err != nil {
			return err
		}
	case "py", "python":
		prog, err := pyast.Inspect(src)
		if err == nil {
			out, err = pyast.Print(prog)
		}
		if err != nil {
			return err
		}
	case "rb", "ruby":
		prog, err := rbast.Inspect(src)
		if err == nil {
			out, err = rbast.Print(prog)
		}
		if err != nil {
			return err
		}
	case "zig":
		prog, err := zigast.Inspect(src)
		if err == nil {
			out, err = zigast.Print(prog)
		}
		if err != nil {
			return err
		}
	case "rs", "rust":
		prog, err := rsast.Inspect(src, rsast.Option{})
		if err == nil {
			out, err = rsast.Print(prog)
		}
		if err != nil {
			return err
		}
	case "scala":
		prog, err := scalaast.Inspect(src)
		if err == nil {
			out, err = scalaast.Print(prog)
		}
		if err != nil {
			return err
		}
	case "scheme":
		prog, err := schemeast.Inspect(src)
		if err == nil {
			out, err = schemeast.Print(prog)
		}
		if err != nil {
			return err
		}
	case "swift":
		prog, err := swiftast.Inspect(src)
		if err == nil {
			out, err = swiftast.Print(prog)
		}
		if err != nil {
			return err
		}
	case "ts", "typescript":
		prog, err := tsast.Inspect(src)
		if err == nil {
			out, err = tsast.Print(prog)
		}
		if err != nil {
			return err
		}
	default:
		return fmt.Errorf("unknown language: %s", cmd.Language)
	}
	fmt.Print(out)
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

func runBuild(cmd *BuildCmd) error {
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
	lang := strings.ToLower(cmd.Target)
	if lang == "" && cmd.Out != "" {
		lang = strings.TrimPrefix(filepath.Ext(cmd.Out), ".")
	}
	data, err := transpileProgram(lang, env, prog, modRoot, cmd.File)
	if err != nil {
		return err
	}
	if cmd.Out != "" {
		return os.WriteFile(cmd.Out, data, 0644)
	}
	_, err = os.Stdout.Write(data)
	return err
}

func runBuildX(cmd *BuildXCmd) error {
	if cmd.Target == "" {
		return fmt.Errorf("target required")
	}
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
	lang := strings.ToLower(cmd.Target)
	data, err := transpileProgram(lang, env, prog, modRoot, cmd.File)
	if err != nil {
		return err
	}
	if cmd.Out != "" {
		return os.WriteFile(cmd.Out, data, 0644)
	}
	_, err = os.Stdout.Write(data)
	return err
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

func transpileProgram(lang string, env *types.Env, prog *parser.Program, root, src string) ([]byte, error) {
	switch lang {
	case "go":
		bench := os.Getenv("MOCHI_BENCHMARK") == "true"
		p, err := gotrans.Transpile(prog, env, bench)
		if err != nil {
			return nil, err
		}
		return gotrans.Emit(p, bench), nil
	case "ts":
		p, err := tstranspiler.Transpile(prog, env, false)
		if err != nil {
			return nil, err
		}
		return tstranspiler.Emit(p), nil
	case "py", "python":
		bench := os.Getenv("MOCHI_BENCHMARK") == "true"
		p, err := py.Transpile(prog, env, bench)
		if err != nil {
			return nil, err
		}
		var buf bytes.Buffer
		if err := py.Emit(&buf, p, bench); err != nil {
			return nil, err
		}
		return buf.Bytes(), nil
	case "clj":
		p, err := cljt.Transpile(prog, env, false)
		if err != nil {
			return nil, err
		}
		return cljt.Format(cljt.EmitString(p)), nil
	case "dart":
		p, err := dartt.Transpile(prog, env, false, false)
		if err != nil {
			return nil, err
		}
		var buf bytes.Buffer
		if err := dartt.Emit(&buf, p); err != nil {
			return nil, err
		}
		return buf.Bytes(), nil
	case "c":
		p, err := ctrans.Transpile(env, prog)
		if err != nil {
			return nil, err
		}
		return p.Emit(), nil
	case "cobol":
		p, err := cobol.Transpile(prog, env)
		if err != nil {
			return nil, err
		}
		return cobol.Emit(p), nil
	case "cpp", "c++":
		p, err := cpp.Transpile(prog, env)
		if err != nil {
			return nil, err
		}
		return p.Emit(), nil
	case "cs", "csharp":
		p, err := cstranspiler.Transpile(prog, env)
		if err != nil {
			return nil, err
		}
		return cstranspiler.Emit(p), nil
	case "erlang", "erl":
		bench := os.Getenv("MOCHI_BENCHMARK") != ""
		p, err := erl.Transpile(prog, env, bench)
		if err != nil {
			return nil, err
		}
		return p.Emit(), nil
	case "ex", "elixir":
		p, err := ex.Transpile(prog, env)
		if err != nil {
			return nil, err
		}
		return ex.Emit(p, false), nil
	case "fortran", "ftn":
		p, err := fortran.Transpile(prog, env)
		if err != nil {
			return nil, err
		}
		return p.Emit(), nil
	case "fs", "fsharp":
		p, err := fstrans.Transpile(prog, env)
		if err != nil {
			return nil, err
		}
		return fstrans.Emit(p), nil
	case "hs", "haskell":
		p, err := hs.Transpile(prog, env, false)
		if err != nil {
			return nil, err
		}
		return hs.Emit(p, false), nil
	case "java":
		p, err := javatr.Transpile(prog, env)
		if err != nil {
			return nil, err
		}
		return javatr.Emit(p), nil
	case "kotlin", "kt":
		p, err := kt.Transpile(env, prog)
		if err != nil {
			return nil, err
		}
		return kt.Emit(p), nil
	case "lua":
		bench := os.Getenv("MOCHI_BENCHMARK") != ""
		p, err := lua.Transpile(prog, env, bench)
		if err != nil {
			return nil, err
		}
		return lua.Emit(p), nil
	case "ocaml":
		p, err := ocaml.Transpile(prog, env)
		if err != nil {
			return nil, err
		}
		return p.Emit(), nil
	case "pas", "pascal":
		p, err := pas.Transpile(env, prog)
		if err != nil {
			return nil, err
		}
		return p.Emit(), nil
	case "php":
		p, err := php.Transpile(prog, env)
		if err != nil {
			return nil, err
		}
		var buf bytes.Buffer
		if err := php.Emit(&buf, p); err != nil {
			return nil, err
		}
		return buf.Bytes(), nil
	case "pl", "prolog":
		p, err := pl.Transpile(prog, env)
		if err != nil {
			return nil, err
		}
		var buf bytes.Buffer
		if err := pl.Emit(&buf, p); err != nil {
			return nil, err
		}
		return buf.Bytes(), nil
	case "racket", "rkt":
		p, err := rkt.Transpile(prog, env, false)
		if err != nil {
			return nil, err
		}
		var buf bytes.Buffer
		if err := rkt.Emit(&buf, p); err != nil {
			return nil, err
		}
		return buf.Bytes(), nil
	case "rb", "ruby":
		p, err := rb.Transpile(prog, env)
		if err != nil {
			return nil, err
		}
		var buf bytes.Buffer
		if err := rb.Emit(&buf, p); err != nil {
			return nil, err
		}
		return buf.Bytes(), nil
	case "rust":
		p, err := rs.Transpile(prog, env, false)
		if err != nil {
			return nil, err
		}
		return rs.Emit(p), nil
	case "scala":
		p, err := scalat.Transpile(prog, env, false)
		if err != nil {
			return nil, err
		}
		return scalat.Emit(p), nil
	case "scheme":
		p, err := scheme.Transpile(prog, env)
		if err != nil {
			return nil, err
		}
		return scheme.Format(scheme.EmitString(p)), nil
	case "st":
		p, err := st.Transpile(prog, env)
		if err != nil {
			return nil, err
		}
		var buf bytes.Buffer
		if err := st.Emit(&buf, p, false); err != nil {
			return nil, err
		}
		return buf.Bytes(), nil
	case "swift":
		p, err := swifttrans.Transpile(env, prog, false)
		if err != nil {
			return nil, err
		}
		return p.Emit(), nil
	case "zig":
		p, err := zigt.Transpile(prog, env, false)
		if err != nil {
			return nil, err
		}
		return p.Emit(), nil
	default:
		return nil, fmt.Errorf("unsupported language: %s", lang)
	}
}

func newRootCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "mochix",
		Short: "Mochix command-line interface",
	}

	cmd.AddCommand(
		newRunCmd(),
		newTestCmd(),
		newBuildCmd(),
		newBuildXCmd(),
		newInitCmd(),
		newGetCmd(),
		newLintCmd(),
		newReplCmd(),
		newLLMCmd(),
		newInferCmd(),
		newInspectCmd(),
		newPrintCmd(),
		newServeCmd(),
		newCheatsheetCmd(),
	)
	return cmd
}

func newRunCmd() *cobra.Command {
	var rc RunCmd
	c := &cobra.Command{
		Use:   "run <file>",
		Short: "Run a Mochi source file",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			rc.File = args[0]
			return runFile(&rc)
		},
	}
	c.Flags().BoolVar(&rc.PrintAST, "ast", false, "Print parsed AST in Lisp format")
	c.Flags().BoolVar(&rc.Debug, "debug", false, "Enable debug output")
	c.Flags().BoolVar(&rc.Memoize, "memo", false, "Enable memoization of pure functions")
	c.Flags().BoolVar(&rc.Fold, "aot", false, "Fold pure calls before execution")
	c.Flags().BoolVar(&rc.PrintIR, "ir", false, "Print VM assembly and exit")
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

func newBuildCmd() *cobra.Command {
	var bc BuildCmd
	c := &cobra.Command{
		Use:   "build <file>",
		Short: "Compile to Go, TypeScript or Python",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			bc.File = args[0]
			return runBuild(&bc)
		},
	}
	c.Flags().StringVarP(&bc.Target, "target", "t", "", "Target language")
	c.Flags().StringVarP(&bc.Out, "out", "o", "", "Output file (default stdout)")
	return c
}

func newBuildXCmd() *cobra.Command {
	var xc BuildXCmd
	c := &cobra.Command{
		Use:   "buildx <file>",
		Short: "Compile to other languages",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			xc.File = args[0]
			return runBuildX(&xc)
		},
	}
	c.Flags().StringVarP(&xc.Target, "target", "t", "", "Target language")
	c.Flags().StringVarP(&xc.Out, "out", "o", "", "Output file (default stdout)")
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

func newInspectCmd() *cobra.Command {
	var ic InspectCmd
	c := &cobra.Command{
		Use:   "inspect <language> <file>",
		Short: "Inspect a source file and print its AST as JSON",
		Args:  cobra.ExactArgs(2),
		RunE: func(cmd *cobra.Command, args []string) error {
			ic.Language = args[0]
			ic.File = args[1]
			return runInspect(&ic)
		},
	}
	return c
}

func newPrintCmd() *cobra.Command {
	var pc PrintCmd
	c := &cobra.Command{
		Use:   "print <language> <file>",
		Short: "Inspect and pretty-print a source file",
		Args:  cobra.ExactArgs(2),
		RunE: func(cmd *cobra.Command, args []string) error {
			pc.Language = args[0]
			pc.File = args[1]
			return runPrint(&pc)
		},
	}
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
