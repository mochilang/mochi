//go:build archive && slow

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

	cast "mochi/archived/aster/x/c"
	cppast "mochi/archived/aster/x/cpp"
	csast "mochi/archived/aster/x/cs"
	dartast "mochi/archived/aster/x/dart"
	elixirast "mochi/archived/aster/x/elixir"
	fsast "mochi/archived/aster/x/fs"
	goast "mochi/archived/aster/x/go"
	haskellast "mochi/archived/aster/x/haskell"
	javaast "mochi/archived/aster/x/java"
	kotlinast "mochi/archived/aster/x/kotlin"
	luaast "mochi/archived/aster/x/lua"
	mochiaster "mochi/archived/aster/x/mochi"
	ocamlast "mochi/archived/aster/x/ocaml"
	pasast "mochi/archived/aster/x/pas"
	phpast "mochi/archived/aster/x/php"
	pyast "mochi/archived/aster/x/py"
	rbast "mochi/archived/aster/x/rb"
	rsast "mochi/archived/aster/x/rs"
	scalaast "mochi/archived/aster/x/scala"
	schemeast "mochi/archived/aster/x/scheme"
	swiftast "mochi/archived/aster/x/swift"
	tsast "mochi/archived/aster/x/ts"
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
			p, err := javaast.Inspect(src, javaast.Options{})
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
