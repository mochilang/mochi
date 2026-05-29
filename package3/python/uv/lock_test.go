package uv

import (
	"strings"
	"testing"
)

const sampleUvLock = `version = 1
requires-python = ">=3.10"

[[package]]
name = "httpx"
version = "0.27.0"
source = { registry = "https://pypi.org/simple" }
dependencies = [
    { name = "anyio" },
    { name = "certifi" },
    { name = "idna", extras = ["all"] },
]

[[package.wheels]]
url = "https://files.pythonhosted.org/packages/abc/httpx-0.27.0-py3-none-any.whl"
hash = "sha256:cafebabecafebabecafebabecafebabecafebabecafebabecafebabecafebabe"

[[package.wheels]]
url = "https://files.pythonhosted.org/packages/def/httpx-0.27.0-py3-none-musllinux_1_2_x86_64.whl"
hash = "sha256:1111111111111111111111111111111111111111111111111111111111111111"

[package.sdist]
url = "https://files.pythonhosted.org/packages/xyz/httpx-0.27.0.tar.gz"
hash = "sha256:deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"

[[package]]
name = "anyio"
version = "4.3.0"
source = { registry = "https://pypi.org/simple" }

[[package]]
name = "private-thing"
version = "1.2.3"
source = { git = "https://example.com/repo.git", rev = "abc123" }

[[package]]
name = "local-pkg"
version = "0.0.1"
source = { path = "../local-pkg" }
`

func TestParseLockfileSample(t *testing.T) {
	lf, err := ParseLockfile([]byte(sampleUvLock))
	if err != nil {
		t.Fatalf("ParseLockfile err = %v", err)
	}
	if lf.Version != 1 {
		t.Errorf("Version = %d", lf.Version)
	}
	if lf.RequiresPython != ">=3.10" {
		t.Errorf("RequiresPython = %q", lf.RequiresPython)
	}
	if len(lf.Packages) != 4 {
		t.Fatalf("len(Packages) = %d; want 4", len(lf.Packages))
	}
	httpx := lf.Packages[0]
	if httpx.Name != "httpx" || httpx.Version != "0.27.0" {
		t.Errorf("httpx = %+v", httpx)
	}
	if httpx.Source.Kind != "registry" || httpx.Source.URL != "https://pypi.org/simple" {
		t.Errorf("httpx.source = %+v", httpx.Source)
	}
	if len(httpx.Dependencies) != 3 {
		t.Errorf("httpx deps = %v", httpx.Dependencies)
	}
	if httpx.Dependencies[2].Name != "idna" || len(httpx.Dependencies[2].Extras) != 1 || httpx.Dependencies[2].Extras[0] != "all" {
		t.Errorf("httpx idna dep = %+v", httpx.Dependencies[2])
	}
	if len(httpx.Wheels) != 2 {
		t.Errorf("len(wheels) = %d", len(httpx.Wheels))
	}
	if httpx.Sdist == nil || !strings.HasSuffix(httpx.Sdist.URL, ".tar.gz") {
		t.Errorf("sdist = %+v", httpx.Sdist)
	}

	priv := lf.Packages[2]
	if priv.Source.Kind != "git" || priv.Source.Reference != "abc123" {
		t.Errorf("git source = %+v", priv.Source)
	}

	local := lf.Packages[3]
	if local.Source.Kind != "path" || local.Source.Path != "../local-pkg" {
		t.Errorf("path source = %+v", local.Source)
	}
}

func TestParseLockfileEmpty(t *testing.T) {
	lf, err := ParseLockfile([]byte(`version = 1`))
	if err != nil {
		t.Fatalf("ParseLockfile err = %v", err)
	}
	if lf.Version != 1 {
		t.Errorf("Version = %d", lf.Version)
	}
	if len(lf.Packages) != 0 {
		t.Errorf("len(Packages) = %d", len(lf.Packages))
	}
}

func TestParseLockfileMissingName(t *testing.T) {
	src := `version = 1
[[package]]
version = "1.0"
`
	_, err := ParseLockfile([]byte(src))
	if err == nil {
		t.Fatal("ParseLockfile err = nil; want missing name")
	}
}

func TestPackagesByName(t *testing.T) {
	lf, _ := ParseLockfile([]byte(sampleUvLock))
	m := lf.PackagesByName()
	if _, ok := m["httpx"]; !ok {
		t.Error("httpx missing from PackagesByName")
	}
	if _, ok := m["anyio"]; !ok {
		t.Error("anyio missing from PackagesByName")
	}
}

func TestSortedPackageNames(t *testing.T) {
	lf, _ := ParseLockfile([]byte(sampleUvLock))
	names := lf.SortedPackageNames()
	want := []string{"anyio", "httpx", "local-pkg", "private-thing"}
	if len(names) != len(want) {
		t.Fatalf("names = %v; want %v", names, want)
	}
	for i, n := range want {
		if names[i] != n {
			t.Errorf("names[%d] = %q; want %q", i, names[i], n)
		}
	}
}

func TestSplitHash(t *testing.T) {
	cases := []struct {
		in       string
		algo     string
		hex      string
		ok       bool
	}{
		{"sha256:cafe", "sha256", "cafe", true},
		{"SHA256:CAFE", "sha256", "cafe", true},
		{"blake3:deadbeef", "blake3", "deadbeef", true},
		{"", "", "", false},
		{"sha256", "", "", false},
		{":cafe", "", "", false},
		{"sha256:", "", "", false},
	}
	for _, tc := range cases {
		a, h, ok := SplitHash(tc.in)
		if ok != tc.ok || a != tc.algo || h != tc.hex {
			t.Errorf("SplitHash(%q) = (%q, %q, %v); want (%q, %q, %v)", tc.in, a, h, ok, tc.algo, tc.hex, tc.ok)
		}
	}
}
