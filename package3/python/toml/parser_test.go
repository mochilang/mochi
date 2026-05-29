package toml

import (
	"strings"
	"testing"
)

func TestParseEmpty(t *testing.T) {
	tree, err := Parse("")
	if err != nil {
		t.Fatalf("Parse(empty) err = %v", err)
	}
	if len(tree) != 0 {
		t.Errorf("Parse(empty) = %v; want empty map", tree)
	}
}

func TestParseScalarTypes(t *testing.T) {
	src := `s = "hello"
n = 42
neg = -17
f = 3.14
fexp = 1.0e3
b = true
nb = false
literal = 'no\nescape'
`
	tree, err := Parse(src)
	if err != nil {
		t.Fatalf("Parse err = %v", err)
	}
	if tree["s"] != "hello" {
		t.Errorf("s = %v; want hello", tree["s"])
	}
	if tree["n"] != int64(42) {
		t.Errorf("n = %v; want 42", tree["n"])
	}
	if tree["neg"] != int64(-17) {
		t.Errorf("neg = %v; want -17", tree["neg"])
	}
	if tree["f"] != 3.14 {
		t.Errorf("f = %v; want 3.14", tree["f"])
	}
	if tree["fexp"] != 1000.0 {
		t.Errorf("fexp = %v; want 1000.0", tree["fexp"])
	}
	if tree["b"] != true || tree["nb"] != false {
		t.Errorf("bools = %v, %v", tree["b"], tree["nb"])
	}
	if tree["literal"] != `no\nescape` {
		t.Errorf("literal = %q; want literal", tree["literal"])
	}
}

func TestParseStringEscapes(t *testing.T) {
	src := `a = "tab\there"
b = "new\nline"
c = "quote: \""
d = "backslash: \\"
e = "unicode: é"
`
	tree, err := Parse(src)
	if err != nil {
		t.Fatalf("Parse err = %v", err)
	}
	if tree["a"] != "tab\there" {
		t.Errorf("a = %q", tree["a"])
	}
	if tree["b"] != "new\nline" {
		t.Errorf("b = %q", tree["b"])
	}
	if tree["c"] != `quote: "` {
		t.Errorf("c = %q", tree["c"])
	}
	if tree["d"] != `backslash: \` {
		t.Errorf("d = %q", tree["d"])
	}
	if tree["e"] != "unicode: é" {
		t.Errorf("e = %q", tree["e"])
	}
}

func TestParseTable(t *testing.T) {
	src := `top = 1
[server]
host = "localhost"
port = 8080

[server.tls]
cert = "/etc/cert"
`
	tree, err := Parse(src)
	if err != nil {
		t.Fatalf("Parse err = %v", err)
	}
	if tree["top"] != int64(1) {
		t.Errorf("top = %v", tree["top"])
	}
	srv, ok := tree["server"].(map[string]any)
	if !ok {
		t.Fatalf("server = %T; want map", tree["server"])
	}
	if srv["host"] != "localhost" || srv["port"] != int64(8080) {
		t.Errorf("server.host/port = %v/%v", srv["host"], srv["port"])
	}
	tls, ok := srv["tls"].(map[string]any)
	if !ok {
		t.Fatalf("server.tls = %T; want map", srv["tls"])
	}
	if tls["cert"] != "/etc/cert" {
		t.Errorf("tls.cert = %v", tls["cert"])
	}
}

func TestParseArrayOfTables(t *testing.T) {
	src := `[[package]]
name = "httpx"
version = "0.27.0"

[[package]]
name = "requests"
version = "2.31.0"
`
	tree, err := Parse(src)
	if err != nil {
		t.Fatalf("Parse err = %v", err)
	}
	pkgs, ok := tree["package"].([]map[string]any)
	if !ok {
		t.Fatalf("package = %T; want []map", tree["package"])
	}
	if len(pkgs) != 2 {
		t.Fatalf("len(packages) = %d; want 2", len(pkgs))
	}
	if pkgs[0]["name"] != "httpx" || pkgs[1]["name"] != "requests" {
		t.Errorf("names = %v, %v", pkgs[0]["name"], pkgs[1]["name"])
	}
}

func TestParseInlineTable(t *testing.T) {
	src := `pos = {x = 1, y = 2}
empty = {}
`
	tree, err := Parse(src)
	if err != nil {
		t.Fatalf("Parse err = %v", err)
	}
	pos, ok := tree["pos"].(map[string]any)
	if !ok {
		t.Fatalf("pos = %T; want map", tree["pos"])
	}
	if pos["x"] != int64(1) || pos["y"] != int64(2) {
		t.Errorf("pos = %v", pos)
	}
	empty, ok := tree["empty"].(map[string]any)
	if !ok || len(empty) != 0 {
		t.Errorf("empty = %v", tree["empty"])
	}
}

func TestParseArrayScalar(t *testing.T) {
	src := `xs = [1, 2, 3]
ss = ["a", "b"]
empty = []
mixed_blank_lines = [
  "a",
  "b",
]
`
	tree, err := Parse(src)
	if err != nil {
		t.Fatalf("Parse err = %v", err)
	}
	xs, ok := tree["xs"].([]any)
	if !ok {
		t.Fatalf("xs = %T", tree["xs"])
	}
	if len(xs) != 3 || xs[0] != int64(1) {
		t.Errorf("xs = %v", xs)
	}
	ss, ok := tree["ss"].([]any)
	if !ok {
		t.Fatalf("ss = %T", tree["ss"])
	}
	if ss[0] != "a" || ss[1] != "b" {
		t.Errorf("ss = %v", ss)
	}
	if mixed, ok := tree["mixed_blank_lines"].([]any); !ok || len(mixed) != 2 {
		t.Errorf("mixed = %v (%T)", tree["mixed_blank_lines"], tree["mixed_blank_lines"])
	}
}

func TestParseArrayOfInlineTables(t *testing.T) {
	src := `deps = [{name = "anyio"}, {name = "certifi"}]`
	tree, err := Parse(src)
	if err != nil {
		t.Fatalf("Parse err = %v", err)
	}
	deps, ok := tree["deps"].([]map[string]any)
	if !ok {
		t.Fatalf("deps = %T; want []map", tree["deps"])
	}
	if len(deps) != 2 || deps[0]["name"] != "anyio" || deps[1]["name"] != "certifi" {
		t.Errorf("deps = %v", deps)
	}
}

func TestParseUvLockLikeFixture(t *testing.T) {
	src := `version = 1
requires-python = ">=3.10"

[[package]]
name = "httpx"
version = "0.27.0"
source = { registry = "https://pypi.org/simple" }
dependencies = [
    { name = "anyio" },
    { name = "certifi" },
]

[[package.wheels]]
url = "https://files.pythonhosted.org/packages/abc/httpx-0.27.0-py3-none-any.whl"
hash = "sha256:cafebabe"

[[package.wheels]]
url = "https://files.pythonhosted.org/packages/def/httpx-0.27.0.tar.gz"
hash = "sha256:deadbeef"

[[package]]
name = "anyio"
version = "4.3.0"
source = { registry = "https://pypi.org/simple" }
`
	tree, err := Parse(src)
	if err != nil {
		t.Fatalf("Parse err = %v", err)
	}
	if tree["version"] != int64(1) {
		t.Errorf("version = %v", tree["version"])
	}
	pkgs, ok := tree["package"].([]map[string]any)
	if !ok {
		t.Fatalf("package = %T", tree["package"])
	}
	if len(pkgs) != 2 {
		t.Fatalf("len(package) = %d", len(pkgs))
	}
	// The [[package.wheels]] entries attach to the first (current) package.
	wheels, ok := pkgs[0]["wheels"].([]map[string]any)
	if !ok {
		t.Fatalf("package[0].wheels = %T", pkgs[0]["wheels"])
	}
	if len(wheels) != 2 {
		t.Fatalf("len(wheels) = %d", len(wheels))
	}
}

func TestParseComments(t *testing.T) {
	src := `# header comment
x = 1 # trailing
# between

[tbl] # at table header
y = 2
`
	tree, err := Parse(src)
	if err != nil {
		t.Fatalf("Parse err = %v", err)
	}
	if tree["x"] != int64(1) {
		t.Errorf("x = %v", tree["x"])
	}
	tbl, ok := tree["tbl"].(map[string]any)
	if !ok {
		t.Fatalf("tbl = %T", tree["tbl"])
	}
	if tbl["y"] != int64(2) {
		t.Errorf("y = %v", tbl["y"])
	}
}

func TestParseRejectsBad(t *testing.T) {
	cases := []struct {
		src, want string
	}{
		{`x = "unterminated`, "unterminated string"},
		{"x = \"newline\nbroken\"", "newline in string"},
		{`x = """multi"""`, "multiline"},
		{`x = '''multi'''`, "multiline"},
		{"x = 0x10", "non-decimal"},
		{"x.y = 1", "dotted keys"},
		{"x = 1\nx = 2", "duplicate key"},
		{"[a]\nx = 1\nx = 2", "duplicate key"},
		{"x = [1,", "unterminated array"},
		{"x = {a = 1", "expected ',' or '}'"},
		{"x = [1 2]", "expected ',' or ']'"},
	}
	for _, tc := range cases {
		_, err := Parse(tc.src)
		if err == nil {
			t.Errorf("Parse(%q) = nil; want error containing %q", tc.src, tc.want)
			continue
		}
		if !strings.Contains(err.Error(), tc.want) {
			t.Errorf("Parse(%q) err = %v; want substring %q", tc.src, err, tc.want)
		}
	}
}

func TestParseQuotedKey(t *testing.T) {
	src := `"with space" = 1
'single' = 2
`
	tree, err := Parse(src)
	if err != nil {
		t.Fatalf("Parse err = %v", err)
	}
	if tree["with space"] != int64(1) {
		t.Errorf("with space = %v", tree["with space"])
	}
	if tree["single"] != int64(2) {
		t.Errorf("single = %v", tree["single"])
	}
}

func TestParseUnderscoreInNumber(t *testing.T) {
	src := `n = 1_000_000`
	tree, err := Parse(src)
	if err != nil {
		t.Fatalf("Parse err = %v", err)
	}
	if tree["n"] != int64(1000000) {
		t.Errorf("n = %v", tree["n"])
	}
}

func TestParseEmptyString(t *testing.T) {
	src := `x = ""
y = ''
`
	tree, err := Parse(src)
	if err != nil {
		t.Fatalf("Parse err = %v", err)
	}
	if tree["x"] != "" {
		t.Errorf("x = %q", tree["x"])
	}
	if tree["y"] != "" {
		t.Errorf("y = %q", tree["y"])
	}
}
