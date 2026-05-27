package build

import (
	"bytes"
	"fmt"
	"net/http"
	"net/http/httptest"
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

// TestPhase14Fetch tests HTTP GET via mochi_fetch (Phase 14.0).
// Uses an in-process httptest server so no internet access is required.
// Gate: fetch expressions compile and produce byte-equal output vs. expected;
// 3 fixtures covering basic GET, string URL in variable, and body length check.
func TestPhase14Fetch(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch r.URL.Path {
		case "/hello":
			fmt.Fprint(w, "hello world")
		case "/lines":
			fmt.Fprint(w, "alpha\nbeta\ngamma")
		case "/empty":
			// 200 with no body
		default:
			http.NotFound(w, r)
		}
	}))
	t.Cleanup(srv.Close)

	cases := []struct {
		name   string
		script string
		want   string
	}{
		{
			name:   "basic_get",
			script: fmt.Sprintf(`fetch "%s/hello" into body`+"\nprint(body)\n", srv.URL),
			want:   "hello world\n",
		},
		{
			name: "url_in_var",
			script: fmt.Sprintf(
				`let url = "%s/lines"`+"\nfetch url into body\nprint(body)\n", srv.URL),
			want: "alpha\nbeta\ngamma\n",
		},
		{
			name:   "empty_body",
			script: fmt.Sprintf(`fetch "%s/empty" into body`+"\nprint(body)\n", srv.URL),
			want:   "\n",
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			runFetchFixture(t, tc.script, tc.want)
		})
	}
}

// TestPhase14_2JSONParse tests json_decode(s) via mochi_json:decode/1 (Phase 14.2).
func TestPhase14_2JSONParse(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		fmt.Fprint(w, `{"name":"alice","age":"30","city":"paris"}`)
	}))
	t.Cleanup(srv.Close)

	script := fmt.Sprintf(`fetch "%s/user" into body`+"\n"+
		`let m = json_decode(body)`+"\n"+
		`print(m["name"])`+"\n"+
		`print(m["city"])`+"\n", srv.URL)

	runFetchFixture(t, script, "alice\nparis\n")
}

// runFetchFixture writes script to a temp .mochi file, compiles it through
// the BEAM pipeline, runs the resulting escript, and checks stdout.
func runFetchFixture(t *testing.T, script, want string) {
	t.Helper()

	dir := t.TempDir()
	mochiPath := filepath.Join(dir, "fixture.mochi")
	if err := os.WriteFile(mochiPath, []byte(script), 0o644); err != nil {
		t.Fatalf("write fixture: %v", err)
	}

	escriptPath := filepath.Join(dir, "fixture.escript")
	d := &Driver{CacheDir: t.TempDir()}
	if err := d.Build(mochiPath, escriptPath, TargetEscript); err != nil {
		t.Fatalf("Driver.Build: %v", err)
	}

	// Use `escript <path>` for Windows compatibility (no shebang support).
	cmd := exec.Command("escript", escriptPath)
	var stdout bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		t.Fatalf("run escript: %v", err)
	}

	got := stdout.String()
	if got != want {
		t.Errorf("stdout mismatch\ngot:  %q\nwant: %q", got, want)
	}
}
