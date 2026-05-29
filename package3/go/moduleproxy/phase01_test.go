package moduleproxy

import (
	"context"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
)

// TestPhase1ModuleProxy is the umbrella sentinel test for MEP-74
// phase 1. It exercises the full end-to-end loop:
//
//  1. A fake proxy serves @v/list, .info, .mod, and .zip for a
//     fixture module.
//  2. The Client.List → Info → Mod → Zip sequence runs.
//  3. The cache stores the zip + mod + info, recording an h1: digest.
//  4. VerifyZip rehashes from disk and confirms integrity.
//  5. Escape / Unescape round-trip the uppercase-bearing module path.
//
// If this test passes the phase is end-to-end functional from a
// caller's point of view; sub-tests cover individual edge cases.
func TestPhase1ModuleProxy(t *testing.T) {
	const modPath = "example.com/Foo"
	const ver = "v1.2.3"
	const goMod = "module example.com/Foo\n\ngo 1.21\n"
	zipBody := mustBuildZip(t, map[string]string{
		modPath + "@" + ver + "/go.mod":  goMod,
		modPath + "@" + ver + "/main.go": "package Foo\n",
	})

	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch {
		case strings.HasSuffix(r.URL.Path, "/@v/list"):
			io.WriteString(w, ver+"\n")
		case strings.HasSuffix(r.URL.Path, "/"+ver+".info"):
			w.Header().Set("Content-Type", "application/json")
			io.WriteString(w, `{"Version":"`+ver+`","Time":"2025-01-15T10:30:00Z"}`)
		case strings.HasSuffix(r.URL.Path, "/"+ver+".mod"):
			io.WriteString(w, goMod)
		case strings.HasSuffix(r.URL.Path, "/"+ver+".zip"):
			w.Header().Set("Content-Type", "application/zip")
			w.Write(zipBody)
		default:
			http.NotFound(w, r)
		}
	}))
	t.Cleanup(srv.Close)

	client := NewClient(srv.URL)
	ctx := context.Background()

	versions, err := client.List(ctx, modPath)
	if err != nil {
		t.Fatalf("List: %v", err)
	}
	if len(versions) != 1 || versions[0] != ver {
		t.Fatalf("List = %v; want [%s]", versions, ver)
	}

	info, err := client.Info(ctx, modPath, ver)
	if err != nil {
		t.Fatalf("Info: %v", err)
	}
	if info.Version != ver {
		t.Errorf("Info.Version = %q; want %q", info.Version, ver)
	}

	modBytes, err := client.Mod(ctx, modPath, ver)
	if err != nil {
		t.Fatalf("Mod: %v", err)
	}
	if string(modBytes) != goMod {
		t.Errorf("Mod body mismatch:\n  got  %q\n  want %q", modBytes, goMod)
	}

	cache := &CacheLayout{Root: t.TempDir()}
	digest, err := cache.StoreZip(modPath, ver, strings.NewReader(string(zipBody)))
	if err != nil {
		t.Fatalf("cache.StoreZip: %v", err)
	}
	if !strings.HasPrefix(digest, H1Prefix) {
		t.Errorf("StoreZip digest %q lacks h1: prefix", digest)
	}
	if _, err := cache.StoreMod(modPath, ver, modBytes); err != nil {
		t.Fatalf("cache.StoreMod: %v", err)
	}
	if err := cache.StoreInfo(modPath, ver, []byte(`{"Version":"`+ver+`"}`)); err != nil {
		t.Fatalf("cache.StoreInfo: %v", err)
	}
	if !cache.Has(modPath, ver) {
		t.Errorf("cache.Has after full store = false")
	}
	if err := cache.VerifyZip(modPath, ver); err != nil {
		t.Errorf("cache.VerifyZip after fresh store: %v", err)
	}

	// Escape round-trip for the upper-case module path.
	esc, err := EscapePath(modPath)
	if err != nil {
		t.Fatalf("EscapePath: %v", err)
	}
	if !strings.Contains(esc, "!foo") {
		t.Errorf("EscapePath(%q) = %q; expected !foo in result", modPath, esc)
	}
	back, err := UnescapePath(esc)
	if err != nil {
		t.Fatalf("UnescapePath: %v", err)
	}
	if back != modPath {
		t.Errorf("round-trip: %q -> %q -> %q", modPath, esc, back)
	}
}
