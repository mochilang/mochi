package vm_test

import (
	"bytes"
	"fmt"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/golden"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func TestVM_ValidPrograms(t *testing.T) {
	golden.Run(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("type error: %v", errs[0])
		}
		p, err := vm.Compile(prog, env)
		if err != nil {
			return nil, fmt.Errorf("compile error: %w", err)
		}
		var out bytes.Buffer
		m := vm.New(p, &out)
		if err := m.Run(); err != nil {
			return nil, fmt.Errorf("run error: %w", err)
		}
		return bytes.TrimSpace(out.Bytes()), nil
	})
}

func TestVM_IR(t *testing.T) {
	golden.Run(t, "tests/vm/valid", ".mochi", ".ir.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("type error: %v", errs[0])
		}
		p, err := vm.Compile(prog, env)
		if err != nil {
			return nil, fmt.Errorf("compile error: %w", err)
		}
		data, err := os.ReadFile(src)
		if err != nil {
			return nil, err
		}
		ir := p.Disassemble(string(data))
		return []byte(ir), nil
	})
}

func TestVM_Fetch(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		fmt.Fprint(w, `{"msg":"ok"}`)
	}))
	defer srv.Close()

	src := fmt.Sprintf("let r = fetch \"%s\"\nprint(r.msg)", srv.URL)
	prog, err := parser.ParseString(src)
	if err != nil {
		t.Fatal(err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	p, err := vm.Compile(prog, env)
	if err != nil {
		t.Fatal(err)
	}
	var out bytes.Buffer
	m := vm.New(p, &out)
	if err := m.Run(); err != nil {
		t.Fatal(err)
	}
	if strings.TrimSpace(out.String()) != "ok" {
		t.Fatalf("unexpected output: %s", out.String())
	}
}

func TestVM_TPCH(t *testing.T) {
	files, err := filepath.Glob("../dataset/tpc-h/*.out")
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatal("no tpch test files")
	}
	for _, want := range files {
		src := strings.TrimSuffix(want, ".out") + ".mochi"
		name := filepath.Base(src)
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			p, err := vm.Compile(prog, env)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			var out bytes.Buffer
			m := vm.New(p, &out)
			if err := m.Run(); err != nil {
				t.Fatalf("run error: %v", err)
			}
			got := strings.TrimSpace(out.String())
			data, err := os.ReadFile(want)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			wantStr := strings.TrimSpace(string(data))
			if got != wantStr {
				t.Errorf("%s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, got, wantStr)
			}
		})
	}
}
