package infer_test

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/interpreter"
	"mochi/parser"
	denoffi "mochi/runtime/ffi/deno"
	goffi "mochi/runtime/ffi/go"
	gopkg "mochi/runtime/ffi/go/testpkg"
	ffiinfo "mochi/runtime/ffi/infer"
	pyffi "mochi/runtime/ffi/python"
	"mochi/runtime/mod"
	"mochi/types"
)

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
				b.WriteString(normalizeType(p.Type))
			}
		} else if p.Type != "" {
			b.WriteString(normalizeType(p.Type))
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
		return ": " + normalizeType(rs[0].Type)
	}
	var b strings.Builder
	b.WriteString(": (")
	for i, r := range rs {
		if i > 0 {
			b.WriteString(", ")
		}
		b.WriteString(normalizeType(r.Type))
	}
	b.WriteString(")")
	return b.String()
}

func externs(info *ffiinfo.ModuleInfo) string {
	alias := parser.AliasFromPath(info.Path)
	var b strings.Builder
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

func normalizeType(t string) string {
	if strings.HasPrefix(t, "untyped ") {
		t = strings.TrimPrefix(t, "untyped ")
	}
	if strings.HasPrefix(t, "[]") {
		return "list<" + normalizeType(t[2:]) + ">"
	}
	if strings.HasPrefix(t, "[") {
		end := strings.Index(t, "]")
		if end > 0 {
			return "list<" + normalizeType(t[end+1:]) + ">"
		}
	}
	if strings.HasPrefix(t, "map[") {
		end := strings.Index(t, "]")
		if end > 0 {
			key := normalizeType(t[4:end])
			val := normalizeType(strings.TrimSpace(t[end+1:]))
			return "map<" + key + ", " + val + ">"
		}
	}
	if strings.HasPrefix(t, "*") {
		return normalizeType(t[1:])
	}
	return t
}

func TestInferFullFlow(t *testing.T) {
	// Setup Python module path
	cwd, _ := os.Getwd()
	pyPath := filepath.Join(cwd, "..", "ffi", "python")
	os.Setenv("PYTHONPATH", pyPath)
	defer os.Unsetenv("PYTHONPATH")

	pyInfo, err := pyffi.Infer("testmod")
	if err != nil {
		t.Fatalf("python infer: %v", err)
	}

	goInfo, err := goffi.Infer("mochi/runtime/ffi/go/testpkg")
	if err != nil {
		t.Fatalf("go infer: %v", err)
	}

	var tsInfo *ffiinfo.ModuleInfo
	if _, err := exec.LookPath("deno"); err == nil {
		tsInfo, err = denoffi.Infer(filepath.Join("..", "ffi", "deno", "testpkg.ts"))
		if err != nil {
			t.Fatalf("ts infer: %v", err)
		}
		// better alias without extension
		tsInfo.Path = "testpkg"
	}

	var src strings.Builder
	src.WriteString("import python \"testmod\" as testmod\n")
	src.WriteString(externs(pyInfo))
	src.WriteString("import go \"mochi/runtime/ffi/go/testpkg\" as testpkg\n")
	src.WriteString(externs(goInfo))
	if tsInfo != nil {
		src.WriteString("import typescript \"./runtime/ffi/deno/testpkg.ts\" as testpkgts\n")
		src.WriteString(externs(tsInfo))
	}
	src.WriteString("print(testmod.add(2,3))\n")
	src.WriteString("print(testmod.PI)\n")
	src.WriteString("print(testpkg.Add(3,4))\n")
	src.WriteString("print(testpkg.Answer)\n")
	if tsInfo != nil {
		src.WriteString("print(testpkgts.add(1,2))\n")
	}

	prog, err := parser.ParseString(src.String())
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}

	prefix := "mochi/runtime/ffi/go/testpkg"
	goffi.Register(prefix+".Pi", gopkg.Pi)
	goffi.Register(prefix+".Answer", gopkg.Answer)
	goffi.Register(prefix+".Add", gopkg.Add)
	goffi.Register(prefix+".Fail", gopkg.Fail)

	modRoot, _ := mod.FindRoot(cwd)
	interp := interpreter.New(prog, env, modRoot)
	var out strings.Builder
	interp.Env().SetWriter(&out)
	if err := interp.Run(); err != nil {
		t.Fatalf("run: %v", err)
	}

	lines := strings.Split(strings.TrimSpace(out.String()), "\n")
	expect := []string{"5", "3.14", "7", "42"}
	if tsInfo != nil {
		expect = append(expect, "3")
	}
	if len(lines) != len(expect) {
		t.Fatalf("unexpected output count %d != %d: %v", len(lines), len(expect), out.String())
	}
	for i, e := range expect {
		if strings.TrimSpace(lines[i]) != e {
			t.Fatalf("line %d = %q, want %q", i+1, lines[i], e)
		}
	}
}
