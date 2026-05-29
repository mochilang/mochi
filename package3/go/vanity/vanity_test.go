package vanity

import (
	"errors"
	"strings"
	"testing"

	"mochi/package3/go/apisurface"
)

func TestIsVanity(t *testing.T) {
	cases := []struct {
		path string
		want bool
	}{
		{"github.com/spf13/cobra", false},
		{"gitlab.com/foo/bar", false},
		{"bitbucket.org/foo/bar", false},
		{"golang.org/x/sync", true},
		{"gopkg.in/yaml.v3", true},
		{"k8s.io/api", true},
		{"google.golang.org/protobuf", true},
		{"sigs.k8s.io/yaml", true},
		{"example.com/foo/bar", true},
		{"", false},
	}
	for _, c := range cases {
		if got := IsVanity(c.path); got != c.want {
			t.Errorf("IsVanity(%q) = %v, want %v", c.path, got, c.want)
		}
	}
}

func TestKnownVCSHostsIsSorted(t *testing.T) {
	for i := 1; i < len(KnownVCSHosts); i++ {
		if KnownVCSHosts[i-1] >= KnownVCSHosts[i] {
			t.Errorf("not sorted at %d: %q >= %q", i, KnownVCSHosts[i-1], KnownVCSHosts[i])
		}
	}
}

func TestResolveDirectGithub(t *testing.T) {
	res, err := Resolve("github.com/spf13/cobra", nil)
	if err != nil {
		t.Fatalf("Resolve: %v", err)
	}
	if res.Module != "github.com/spf13/cobra" {
		t.Errorf("Module = %q", res.Module)
	}
	if res.VCS != "git" {
		t.Errorf("VCS = %q", res.VCS)
	}
	if res.RepoURL != "https://github.com/spf13/cobra" {
		t.Errorf("RepoURL = %q", res.RepoURL)
	}
}

func TestResolveDirectGithubSubpackage(t *testing.T) {
	// A subpackage path resolves to the module root, not the
	// deeper path.
	res, err := Resolve("github.com/spf13/cobra/internal/util", nil)
	if err != nil {
		t.Fatalf("Resolve: %v", err)
	}
	if res.Module != "github.com/spf13/cobra" {
		t.Errorf("Module = %q (subpackage should yield root)", res.Module)
	}
}

func TestResolveEmptyPath(t *testing.T) {
	_, err := Resolve("", nil)
	if !errors.Is(err, ErrVanity) {
		t.Fatalf("want ErrVanity, got %v", err)
	}
}

func TestResolveVanityNoFetcher(t *testing.T) {
	_, err := Resolve("golang.org/x/sync", nil)
	if !errors.Is(err, ErrVanity) {
		t.Fatalf("want ErrVanity, got %v", err)
	}
}

func TestResolveVanityViaMapFetcher(t *testing.T) {
	fetcher := MapFetcher{
		"https://golang.org/x/sync?go-get=1": `<!doctype html>
<html><head>
<meta name="go-import" content="golang.org/x/sync git https://go.googlesource.com/sync">
</head></html>`,
	}
	res, err := Resolve("golang.org/x/sync", fetcher)
	if err != nil {
		t.Fatalf("Resolve: %v", err)
	}
	if res.Module != "golang.org/x/sync" {
		t.Errorf("Module = %q", res.Module)
	}
	if res.VCS != "git" {
		t.Errorf("VCS = %q", res.VCS)
	}
	if res.RepoURL != "https://go.googlesource.com/sync" {
		t.Errorf("RepoURL = %q", res.RepoURL)
	}
}

func TestResolveVanityLongestPrefixWins(t *testing.T) {
	// Two meta tags; the more-specific one should win.
	body := `<meta name="go-import" content="example.com git https://git.example.com/main">
<meta name="go-import" content="example.com/x/sync git https://git.example.com/sync">`
	res, err := ParseGoImport(body, "example.com/x/sync/internal")
	if err != nil {
		t.Fatalf("ParseGoImport: %v", err)
	}
	if res.Module != "example.com/x/sync" {
		t.Errorf("Module = %q, want longest prefix", res.Module)
	}
}

func TestResolveVanityRejectsNoMeta(t *testing.T) {
	fetcher := MapFetcher{
		"https://golang.org/x/sync?go-get=1": `<html><head><title>no meta</title></head></html>`,
	}
	_, err := Resolve("golang.org/x/sync", fetcher)
	if !errors.Is(err, ErrVanity) {
		t.Fatalf("want ErrVanity, got %v", err)
	}
}

func TestResolveVanityRejectsNonCoveringMeta(t *testing.T) {
	fetcher := MapFetcher{
		"https://golang.org/x/sync?go-get=1": `<meta name="go-import" content="completely.different git https://elsewhere">`,
	}
	_, err := Resolve("golang.org/x/sync", fetcher)
	if !errors.Is(err, ErrVanity) {
		t.Fatalf("want ErrVanity, got %v", err)
	}
}

func TestParseGoImportSingleQuoted(t *testing.T) {
	body := `<meta name='go-import' content='golang.org/x/sync git https://go.googlesource.com/sync'>`
	res, err := ParseGoImport(body, "golang.org/x/sync")
	if err != nil {
		t.Fatalf("ParseGoImport: %v", err)
	}
	if res.RepoURL != "https://go.googlesource.com/sync" {
		t.Errorf("RepoURL = %q", res.RepoURL)
	}
}

func TestParseGoImportAttrOrderIndependent(t *testing.T) {
	body := `<meta content="golang.org/x/sync git https://go.googlesource.com/sync" name="go-import">`
	res, err := ParseGoImport(body, "golang.org/x/sync")
	if err != nil {
		t.Fatalf("ParseGoImport: %v", err)
	}
	if res.Module != "golang.org/x/sync" {
		t.Errorf("Module = %q", res.Module)
	}
}

func TestParseGoImportIgnoresMalformedContent(t *testing.T) {
	body := `<meta name="go-import" content="onlytwo fields">
<meta name="go-import" content="golang.org/x/sync git https://go.googlesource.com/sync">`
	res, err := ParseGoImport(body, "golang.org/x/sync")
	if err != nil {
		t.Fatalf("ParseGoImport: %v", err)
	}
	if res.Module != "golang.org/x/sync" {
		t.Errorf("Module = %q", res.Module)
	}
}

func TestParseGoImportIgnoresOtherMetas(t *testing.T) {
	body := `<meta name="generator" content="hugo">
<meta name="description" content="some module">
<meta name="go-import" content="golang.org/x/sync git https://go.googlesource.com/sync">`
	res, err := ParseGoImport(body, "golang.org/x/sync")
	if err != nil {
		t.Fatalf("ParseGoImport: %v", err)
	}
	if res.VCS != "git" {
		t.Errorf("VCS = %q", res.VCS)
	}
}

func TestExtractAttrUnquoted(t *testing.T) {
	if got := extractAttr(`<meta name=go-import>`, "name"); got != "" {
		t.Errorf("unquoted attr should be ignored, got %q", got)
	}
}

func TestMapFetcherMiss(t *testing.T) {
	_, err := MapFetcher{}.Fetch("http://example.com")
	if !errors.Is(err, ErrVanity) {
		t.Fatalf("want ErrVanity, got %v", err)
	}
}

func TestWasmTargetIsValid(t *testing.T) {
	if !WasmTargetWasip1.IsValid() {
		t.Errorf("wasip1 should be valid")
	}
	if !WasmTargetJS.IsValid() {
		t.Errorf("wasm-js should be valid")
	}
	if WasmTarget("garbage").IsValid() {
		t.Errorf("garbage should be invalid")
	}
}

func TestCheckPublishWasip1BansSyscallJS(t *testing.T) {
	pkg := apisurface.Package{
		ImportPath: "x",
		Imports:    []string{"fmt", "syscall/js"},
	}
	got := CheckPublish(WasmTargetWasip1, pkg)
	if len(got) != 1 || got[0].Where != "syscall/js" {
		t.Fatalf("want syscall/js banned on wasip1, got %v", got)
	}
}

func TestCheckPublishJSAllowsSyscallJS(t *testing.T) {
	pkg := apisurface.Package{
		ImportPath: "x",
		Imports:    []string{"fmt", "syscall/js"},
	}
	got := CheckPublish(WasmTargetJS, pkg)
	if len(got) != 0 {
		t.Fatalf("syscall/js should be OK on wasm-js, got %v", got)
	}
}

func TestCheckPublishBansCgo(t *testing.T) {
	pkg := apisurface.Package{
		ImportPath: "x",
		Imports:    []string{"runtime/cgo"},
	}
	for _, target := range []WasmTarget{WasmTargetWasip1, WasmTargetJS} {
		got := CheckPublish(target, pkg)
		if len(got) != 1 || got[0].Where != "runtime/cgo" {
			t.Errorf("%s: want cgo banned, got %v", target, got)
		}
	}
}

func TestCheckPublishBansDebugFamily(t *testing.T) {
	pkg := apisurface.Package{
		ImportPath: "x",
		Imports:    []string{"debug/elf", "debug/macho", "debug/pe"},
	}
	got := CheckPublish(WasmTargetWasip1, pkg)
	if len(got) != 3 {
		t.Fatalf("want 3 violations, got %v", got)
	}
	want := []string{"debug/elf", "debug/macho", "debug/pe"}
	for i, w := range want {
		if got[i].Where != w {
			t.Errorf("got[%d].Where = %q, want %q", i, got[i].Where, w)
		}
	}
}

func TestCheckPublishInvalidTargetIsNoop(t *testing.T) {
	if got := CheckPublish("garbage", apisurface.Package{Imports: []string{"runtime/cgo"}}); got != nil {
		t.Errorf("invalid target should yield no violations, got %v", got)
	}
}

func TestIsPublishable(t *testing.T) {
	pure := apisurface.Package{ImportPath: "x", Imports: []string{"fmt"}}
	if !IsPublishable(WasmTargetWasip1, pure) {
		t.Errorf("pure pkg should be publishable")
	}
	bad := apisurface.Package{ImportPath: "x", Imports: []string{"runtime/cgo"}}
	if IsPublishable(WasmTargetWasip1, bad) {
		t.Errorf("cgo pkg should not be publishable")
	}
}

func TestViolationString(t *testing.T) {
	v := Violation{Kind: "import", Where: "runtime/cgo", Reason: "no cgo on wasm"}
	got := v.String()
	if !strings.Contains(got, "import:") || !strings.Contains(got, "runtime/cgo") {
		t.Errorf("violation string = %q", got)
	}
}
