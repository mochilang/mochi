package main

import (
	"fmt"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strconv"
	"strings"

	"github.com/alexflint/go-arg"

	ccode "mochi/compile/c"
	cljcode "mochi/compile/clj"
	cobolcode "mochi/compile/cobol"
	cppcode "mochi/compile/cpp"
	cscode "mochi/compile/cs"
	dartcode "mochi/compile/dart"
	erlcode "mochi/compile/erlang"
	excode "mochi/compile/ex"
	ftncode "mochi/compile/fortran"
	fscode "mochi/compile/fs"
	gocode "mochi/compile/go"
	hscode "mochi/compile/hs"
	javacode "mochi/compile/java"
	jvmcode "mochi/compile/jvm"
	ktcode "mochi/compile/kt"
	luacode "mochi/compile/lua"
	mlcode "mochi/compile/ocaml"
	pascode "mochi/compile/pas"
	phpcode "mochi/compile/php"
	plcode "mochi/compile/pl"
	pycode "mochi/compile/py"
	rbcode "mochi/compile/rb"
	rktcode "mochi/compile/rkt"
	rscode "mochi/compile/rust"
	scalacode "mochi/compile/scala"
	schemecode "mochi/compile/scheme"
	stcode "mochi/compile/st"
	swiftcode "mochi/compile/swift"
	tscode "mochi/compile/ts"
	wasmcode "mochi/compile/wasm"
	zigcode "mochi/compile/zig"
	"mochi/interpreter"
	"mochi/parser"
	"mochi/runtime/mod"
	"mochi/types"
)

// CLI defines subcommands for building and running LeetCode solutions.
type CLI struct {
	Build *BuildCmd `arg:"subcommand:build" help:"Compile solutions"`
	Run   *RunCmd   `arg:"subcommand:run" help:"Run solutions with interpreter"`
	Test  *TestCmd  `arg:"subcommand:test" help:"Run test blocks"`
}

type BuildCmd struct {
	ID   int      `arg:"-i,--id" help:"Single problem id"`
	From int      `arg:"--from" help:"Start id"`
	To   int      `arg:"--to" help:"End id"`
	Lang []string `arg:"-l,--lang,separate" help:"Target language (repeatable)"`
	All  bool     `arg:"--all" help:"Build for all languages"`
	Run  bool     `arg:"--run" help:"Execute compiled code"`
}

type RunCmd struct {
	ID int `arg:"positional,required" help:"Problem id"`
}

type TestCmd struct {
	ID int `arg:"positional" help:"Problem id (default all)"`
}

func main() {
	var cli CLI
	arg.MustParse(&cli)

	switch {
	case cli.Build != nil:
		if err := cli.Build.RunBuild(); err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
	case cli.Run != nil:
		if err := runProblem(cli.Run.ID); err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
	case cli.Test != nil:
		if err := runTests(cli.Test.ID); err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
	default:
		arg.MustParse(&cli).WriteHelp(os.Stdout)
	}
}

func (c *BuildCmd) ids() []int {
	if c.ID > 0 {
		return []int{c.ID}
	}
	from := c.From
	to := c.To
	if from == 0 && to == 0 {
		return nil
	}
	if from == 0 {
		from = 1
	}
	if to == 0 {
		to = from
	}
	var ids []int
	for i := from; i <= to; i++ {
		ids = append(ids, i)
	}
	return ids
}

func (c *BuildCmd) languages() []string {
	if c.All {
		return allCompileLanguages()
	}
	if len(c.Lang) == 0 {
		return []string{"go"}
	}
	return c.Lang
}

func (c *BuildCmd) RunBuild() error {
	ids := c.ids()
	if len(ids) == 0 {
		return fmt.Errorf("no ids specified")
	}
	langs := c.languages()

	for _, id := range ids {
		dir := filepath.Join("examples", "leetcode", strconv.Itoa(id))
		entries, err := os.ReadDir(dir)
		if err != nil {
			fmt.Fprintf(os.Stderr, "skip %d: %v\n", id, err)
			continue
		}
		for _, e := range entries {
			if e.IsDir() || !strings.HasSuffix(e.Name(), ".mochi") {
				continue
			}
			src := filepath.Join(dir, e.Name())
			for _, lang := range langs {
				if err := buildOne(src, lang, c.Run); err != nil {
					fmt.Fprintf(os.Stderr, "%v\n", err)
				}
			}
		}
	}
	return nil
}

func buildOne(src, lang string, run bool) error {
	prog, err := parser.Parse(src)
	if err != nil {
		return fmt.Errorf("%s: %v", src, err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return fmt.Errorf("%s: %v", src, errs[0])
	}
	modRoot, _ := mod.FindRoot(filepath.Dir(src))

	base := strings.TrimSuffix(filepath.Base(src), ".mochi")
	outDir := filepath.Join("examples", "leetcode-out", lang, filepath.Base(filepath.Dir(src)))
	if err := os.MkdirAll(outDir, 0755); err != nil {
		return err
	}
	outFile := filepath.Join(outDir, base+"."+lang)
	var data []byte
	switch lang {
	case "go":
		data, err = gocode.New(env).Compile(prog)
	case "py":
		data, err = pycode.New(env).Compile(prog)
	case "ts":
		data, err = tscode.New(env, modRoot).Compile(prog)
	case "cpp":
		data, err = cppcode.New(env).Compile(prog)
	case "c":
		data, err = ccode.New(env).Compile(prog)
	case "clj":
		data, err = cljcode.New(env).Compile(prog)
	case "cobol":
		data, err = cobolcode.New(env).Compile(prog)
	case "cs":
		data, err = cscode.New(env).Compile(prog)
	case "dart":
		data, err = dartcode.New(env).Compile(prog)
	case "erlang":
		data, err = erlcode.New(env).Compile(prog)
	case "ex":
		data, err = excode.New(env).Compile(prog)
	case "fortran":
		data, err = ftncode.New().Compile(prog)
	case "fs":
		data, err = fscode.New(env).Compile(prog)
	case "hs":
		data, err = hscode.New(env).Compile(prog)
	case "java":
		data, err = javacode.New(env).Compile(prog)
	case "jvm":
		data, err = jvmcode.New(env).Compile(prog)
	case "kt":
		data, err = ktcode.New(env).Compile(prog)
	case "lua":
		data, err = luacode.New(env).Compile(prog)
	case "ocaml":
		data, err = mlcode.New(env).Compile(prog)
	case "pas":
		data, err = pascode.New(env).Compile(prog)
	case "php":
		data, err = phpcode.New(env).Compile(prog)
	case "pl":
		data, err = plcode.New(env).Compile(prog)
	case "rb":
		data, err = rbcode.New(env).Compile(prog)
	case "rkt":
		data, err = rktcode.New(env).Compile(prog)
	case "rust":
		data, err = rscode.New(env).Compile(prog)
	case "scala":
		data, err = scalacode.New(env).Compile(prog)
	case "scheme":
		data, err = schemecode.New(env).Compile(prog)
	case "st":
		data, err = stcode.New(env).Compile(prog)
	case "swift":
		data, err = swiftcode.New(env).Compile(prog)
	case "wasm":
		data, err = wasmcode.New(env).Compile(prog)
	case "zig":
		data, err = zigcode.New(env).Compile(prog)
	default:
		return fmt.Errorf("unsupported language: %s", lang)
	}
	if err != nil {
		return fmt.Errorf("compile %s to %s: %v", src, lang, err)
	}
	if err := os.WriteFile(outFile, data, 0644); err != nil {
		return err
	}
	fmt.Printf("generated %s\n", outFile)
	if run {
		return runOutput(outFile, lang)
	}
	return nil
}

func runOutput(file, lang string) error {
	switch lang {
	case "go":
		cmd := exec.Command("go", "run", file)
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		return cmd.Run()
	case "py":
		cmd := exec.Command("python3", file)
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		return cmd.Run()
	case "ts":
		cmd := exec.Command("deno", "run", "--allow-all", file)
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		return cmd.Run()
	case "cpp":
		exe := strings.TrimSuffix(file, ".cpp")
		if out, err := exec.Command("g++", file, "-std=c++17", "-o", exe).CombinedOutput(); err != nil {
			return fmt.Errorf("g++: %v\n%s", err, string(out))
		}
		cmd := exec.Command(exe)
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		return cmd.Run()
	case "scala":
		dir := filepath.Dir(file)
		cmd := exec.Command("scalac", filepath.Base(file))
		cmd.Dir = dir
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			return err
		}
		runCmd := exec.Command("scala", "Main")
		runCmd.Dir = dir
		runCmd.Stdout = os.Stdout
		runCmd.Stderr = os.Stderr
		return runCmd.Run()
	case "java":
		if err := javacode.EnsureJavac(); err != nil {
			return err
		}
		dir := filepath.Dir(file)
		cmd := exec.Command("javac", filepath.Base(file))
		cmd.Dir = dir
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			return err
		}
		runCmd := exec.Command("java", "-cp", dir, "Main")
		runCmd.Stdout = os.Stdout
		runCmd.Stderr = os.Stderr
		return runCmd.Run()
	case "kt":
		if err := ktcode.EnsureKotlin(); err != nil {
			return err
		}
		dir := filepath.Dir(file)
		jar := filepath.Join(dir, strings.TrimSuffix(filepath.Base(file), ".kt")+".jar")
		cmd := exec.Command("kotlinc", file, "-include-runtime", "-d", jar)
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			return err
		}
		runCmd := exec.Command("java", "-jar", jar)
		runCmd.Stdout = os.Stdout
		runCmd.Stderr = os.Stderr
		return runCmd.Run()
	case "fortran":
		gfortran, err := ftncode.EnsureFortran()
		if err != nil {
			return err
		}
		exe := strings.TrimSuffix(file, ".f90")
		if out, err := exec.Command(gfortran, file, "-o", exe).CombinedOutput(); err != nil {
			return fmt.Errorf("gfortran: %v\n%s", err, string(out))
		}
		cmd := exec.Command(exe)
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		return cmd.Run()
	case "swift":
		exe := strings.TrimSuffix(file, ".swift")
		if out, err := exec.Command("swiftc", file, "-o", exe).CombinedOutput(); err != nil {
			return fmt.Errorf("swiftc: %v\n%s", err, string(out))
		}
		cmd := exec.Command(exe)
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		return cmd.Run()
	default:
		return fmt.Errorf("no runner for %s", lang)
	}
}

func runProblem(id int) error {
	dir := filepath.Join("examples", "leetcode", strconv.Itoa(id))
	files, err := filepath.Glob(filepath.Join(dir, "*.mochi"))
	if err != nil {
		return err
	}
	for _, f := range files {
		if err := runFile(f); err != nil {
			return err
		}
	}
	return nil
}

func runFile(file string) error {
	prog, err := parser.Parse(file)
	if err != nil {
		return err
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return fmt.Errorf("%s: %v", file, errs[0])
	}
	modRoot, _ := mod.FindRoot(filepath.Dir(file))
	interp := interpreter.New(prog, env, modRoot)
	return interp.Run()
}

func runTests(id int) error {
	var files []string
	if id > 0 {
		dir := filepath.Join("examples", "leetcode", strconv.Itoa(id))
		fsys := os.DirFS(dir)
		fs.WalkDir(fsys, ".", func(path string, d fs.DirEntry, err error) error {
			if err != nil || d.IsDir() {
				return err
			}
			if strings.HasSuffix(path, ".mochi") {
				files = append(files, filepath.Join(dir, path))
			}
			return nil
		})
	} else {
		fs.WalkDir(os.DirFS("examples/leetcode"), ".", func(path string, d fs.DirEntry, err error) error {
			if err != nil || d.IsDir() {
				return err
			}
			if strings.HasSuffix(path, ".mochi") {
				files = append(files, filepath.Join("examples/leetcode", path))
			}
			return nil
		})
	}
	for _, f := range files {
		if err := testFile(f); err != nil {
			return err
		}
	}
	return nil
}

func testFile(file string) error {
	prog, err := parser.Parse(file)
	if err != nil {
		return err
	}
	env := types.NewEnv(nil)
	modRoot, _ := mod.FindRoot(filepath.Dir(file))
	if errs := types.Check(prog, env); len(errs) > 0 {
		return fmt.Errorf("%s: %v", file, errs[0])
	}
	interp := interpreter.New(prog, env, modRoot)
	return interp.Test()
}

func allCompileLanguages() []string {
	entries, err := os.ReadDir("compile")
	if err != nil {
		return []string{"go"}
	}
	langs := make([]string, 0, len(entries))
	for _, e := range entries {
		if e.IsDir() {
			langs = append(langs, e.Name())
		}
	}
	sort.Strings(langs)
	return langs
}
