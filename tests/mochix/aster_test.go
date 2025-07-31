//go:build slow

package mochix_test

import (
	"bytes"
	"encoding/json"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	cast "mochi/aster/x/c"
	cppast "mochi/aster/x/cpp"
	csast "mochi/aster/x/cs"
	dartast "mochi/aster/x/dart"
	elixirast "mochi/aster/x/elixir"
	fsast "mochi/aster/x/fs"
	goast "mochi/aster/x/go"
	haskellast "mochi/aster/x/haskell"
	javaast "mochi/aster/x/java"
	kotlinast "mochi/aster/x/kotlin"
	luaast "mochi/aster/x/lua"
	mochiaster "mochi/aster/x/mochi"
	ocamlast "mochi/aster/x/ocaml"
	pasast "mochi/aster/x/pas"
	phpast "mochi/aster/x/php"
	pyast "mochi/aster/x/py"
	rbast "mochi/aster/x/rb"
	rsast "mochi/aster/x/rs"
	scalaast "mochi/aster/x/scala"
	schemeast "mochi/aster/x/scheme"
	swiftast "mochi/aster/x/swift"
	tsast "mochi/aster/x/ts"
)

func TestInspectGolden(t *testing.T) {
	root := repoRoot(t)
	asterDir := filepath.Join(root, "tests/aster/x")
	inspectMap := map[string]func(string) ([]byte, error){
		"c": func(src string) ([]byte, error) {
			p, err := cast.Inspect(src)
			if err != nil {
				return nil, err
			}
			return json.MarshalIndent(p, "", "  ")
		},
		"cpp": func(src string) ([]byte, error) {
			p, err := cppast.Inspect(src)
			if err != nil {
				return nil, err
			}
			return json.MarshalIndent(p, "", "  ")
		},
		"cs": func(src string) ([]byte, error) {
			p, err := csast.Inspect(src)
			if err != nil {
				return nil, err
			}
			return json.MarshalIndent(p, "", "  ")
		},
		"dart": func(src string) ([]byte, error) {
			p, err := dartast.Inspect(src)
			if err != nil {
				return nil, err
			}
			return json.MarshalIndent(p, "", "  ")
		},
		"elixir": func(src string) ([]byte, error) {
			p, err := elixirast.Inspect(src)
			if err != nil {
				return nil, err
			}
			return json.MarshalIndent(p, "", "  ")
		},
		"fs": func(src string) ([]byte, error) {
			p, err := fsast.Inspect(src)
			if err != nil {
				return nil, err
			}
			return json.MarshalIndent(p, "", "  ")
		},
		"go": func(src string) ([]byte, error) {
			p, err := goast.Inspect(src)
			if err != nil {
				return nil, err
			}
			return json.MarshalIndent(p, "", "  ")
		},
		"haskell": func(src string) ([]byte, error) {
			p, err := haskellast.Inspect(src)
			if err != nil {
				return nil, err
			}
			return json.MarshalIndent(p, "", "  ")
		},
		"java": func(src string) ([]byte, error) {
			p, err := javaast.Inspect(src, false)
			if err != nil {
				return nil, err
			}
			return json.MarshalIndent(p, "", "  ")
		},
		"kotlin": func(src string) ([]byte, error) {
			p, err := kotlinast.Inspect(src)
			if err != nil {
				return nil, err
			}
			return json.MarshalIndent(p, "", "  ")
		},
		"lua": func(src string) ([]byte, error) {
			p, err := luaast.Inspect(src)
			if err != nil {
				return nil, err
			}
			return json.MarshalIndent(p, "", "  ")
		},
		"mochi": func(src string) ([]byte, error) {
			p, err := mochiaster.Inspect(src)
			if err != nil {
				return nil, err
			}
			return json.MarshalIndent(p, "", "  ")
		},
		"ocaml": func(src string) ([]byte, error) {
			p, err := ocamlast.Inspect(src)
			if err != nil {
				return nil, err
			}
			return json.MarshalIndent(p, "", "  ")
		},
		"pas": func(src string) ([]byte, error) {
			p, err := pasast.Inspect(src)
			if err != nil {
				return nil, err
			}
			return json.MarshalIndent(p, "", "  ")
		},
		"php": func(src string) ([]byte, error) {
			p, err := phpast.Inspect(src, nil)
			if err != nil {
				return nil, err
			}
			return json.MarshalIndent(p, "", "  ")
		},
		"py": func(src string) ([]byte, error) {
			p, err := pyast.Inspect(src)
			if err != nil {
				return nil, err
			}
			return json.MarshalIndent(p, "", "  ")
		},
		"ruby": func(src string) ([]byte, error) {
			p, err := rbast.Inspect(src)
			if err != nil {
				return nil, err
			}
			return json.MarshalIndent(p, "", "  ")
		},
		"rs": func(src string) ([]byte, error) {
			p, err := rsast.Inspect(src, rsast.Option{})
			if err != nil {
				return nil, err
			}
			return json.MarshalIndent(p, "", "  ")
		},
		"scala": func(src string) ([]byte, error) {
			p, err := scalaast.Inspect(src)
			if err != nil {
				return nil, err
			}
			return json.MarshalIndent(p, "", "  ")
		},
		"scheme": func(src string) ([]byte, error) {
			p, err := schemeast.Inspect(src)
			if err != nil {
				return nil, err
			}
			return json.MarshalIndent(p, "", "  ")
		},
		"swift": func(src string) ([]byte, error) {
			p, err := swiftast.Inspect(src)
			if err != nil {
				return nil, err
			}
			return json.MarshalIndent(p, "", "  ")
		},
		"ts": func(src string) ([]byte, error) {
			p, err := tsast.Inspect(src)
			if err != nil {
				return nil, err
			}
			return json.MarshalIndent(p, "", "  ")
		},
	}
	langMap := map[string]struct{ dir, ext string }{
		"elixir":  {"ex", "exs"},
		"erlang":  {"erl", "erl"},
		"haskell": {"hs", "hs"},
		"kotlin":  {"kt", "kt"},
		"ocaml":   {"ocaml", "ml"},
		"prolog":  {"prolog", "pl"},
		"ruby":    {"rb", "rb"},
		"scheme":  {"scheme", "scm"},
	}

	entries, err := os.ReadDir(asterDir)
	if err != nil {
		t.Fatal(err)
	}
	for _, ent := range entries {
		lang := ent.Name()
		if !ent.IsDir() || lang == "rkt" {
			continue
		}
		cfg, ok := langMap[lang]
		if !ok {
			cfg = struct{ dir, ext string }{lang, lang}
		}

		gfiles, err := filepath.Glob(filepath.Join(asterDir, lang, "*.json"))
		if err != nil {
			t.Fatal(err)
		}
		sort.Strings(gfiles)
		for _, gf := range gfiles {
			base := filepath.Base(gf)
			nameExt := strings.TrimSuffix(base, ".json")
			i := strings.LastIndex(nameExt, ".")
			if i < 0 {
				t.Fatalf("invalid golden filename: %s", base)
			}
			name := nameExt[:i]
			src := filepath.Join(root, "tests/transpiler/x", cfg.dir, name+"."+cfg.ext)
			if _, err := os.Stat(src); os.IsNotExist(err) {
				t.Skipf("source %s not found", src)
				continue
			}
			fn, ok := inspectMap[lang]
			if !ok {
				t.Skipf("inspect for %s not supported", lang)
				continue
			}
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			got, err := fn(string(data))
			if err != nil {
				if ee, ok := err.(*exec.Error); ok {
					t.Skipf("%s not installed", ee.Name)
					continue
				}
				t.Fatalf("inspect error: %v", err)
			}
			want, err := os.ReadFile(gf)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			want = bytes.TrimSpace(want)
			if !bytes.Equal(got, want) {
				t.Fatalf("golden mismatch for %s\n--- got ---\n%s\n--- want ---\n%s", base, got, want)
			}
		}
	}
}
