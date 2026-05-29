package uv

import (
	"context"
	"reflect"
	"testing"
)

func TestLockOptionsBuild(t *testing.T) {
	cases := []struct {
		name string
		opts LockOptions
		want []string
	}{
		{"defaults", LockOptions{}, nil},
		{"highest", LockOptions{Resolution: "highest"}, nil},
		{"lowest", LockOptions{Resolution: "lowest"}, []string{"--resolution", "lowest"}},
		{"lowest-direct", LockOptions{Resolution: "lowest-direct"}, []string{"--resolution", "lowest-direct"}},
		{"python-version", LockOptions{PythonVersion: "3.12"}, []string{"--python", "3.12"}},
		{"index-url", LockOptions{IndexURL: "https://pypi.org/simple"}, []string{"--index-url", "https://pypi.org/simple"}},
		{"extra-indexes",
			LockOptions{ExtraIndexURLs: []string{"https://a", "https://b"}},
			[]string{"--extra-index-url", "https://a", "--extra-index-url", "https://b"}},
		{"no-build", LockOptions{NoBuild: true}, []string{"--no-build"}},
		{"extra-args", LockOptions{ExtraArgs: []string{"--frozen"}}, []string{"--frozen"}},
		{"combined",
			LockOptions{PythonVersion: "3.13", Resolution: "lowest", IndexURL: "https://pypi.org/simple", NoBuild: true},
			[]string{"--python", "3.13", "--resolution", "lowest", "--index-url", "https://pypi.org/simple", "--no-build"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			got := tc.opts.BuildLockArgs()
			if !reflect.DeepEqual(got, tc.want) {
				t.Errorf("got %q want %q", got, tc.want)
			}
		})
	}
}

// fakeRunner satisfies Runner without spawning a process.
type fakeRunner struct {
	runs        []fakeCall
	stdout      []byte
	versionStr  string
	versionErr  error
	runErr      error
}

type fakeCall struct {
	workDir string
	args    []string
}

func (f *fakeRunner) Run(ctx context.Context, workDir string, env []string, args ...string) ([]byte, error) {
	f.runs = append(f.runs, fakeCall{workDir: workDir, args: args})
	if f.runErr != nil {
		return nil, f.runErr
	}
	return f.stdout, nil
}

func (f *fakeRunner) Version(ctx context.Context) (string, error) {
	return f.versionStr, f.versionErr
}

func TestExport(t *testing.T) {
	fake := &fakeRunner{stdout: []byte("# pylock body")}
	out, err := Export(context.Background(), fake, "/tmp/proj")
	if err != nil {
		t.Fatalf("Export err = %v", err)
	}
	if string(out) != "# pylock body" {
		t.Errorf("Export stdout = %q", out)
	}
	if len(fake.runs) != 1 {
		t.Fatalf("runs = %d; want 1", len(fake.runs))
	}
	if fake.runs[0].workDir != "/tmp/proj" {
		t.Errorf("workDir = %q", fake.runs[0].workDir)
	}
	want := []string{"export", "--format", "pylock.toml"}
	if !reflect.DeepEqual(fake.runs[0].args, want) {
		t.Errorf("args = %q; want %q", fake.runs[0].args, want)
	}
}

func TestExecRunnerLocateMissing(t *testing.T) {
	// We can't reliably simulate uv missing on a host that has uv installed,
	// so we exercise the not-installed path indirectly: Locate must return
	// either a path string or a "not found" error, never a nil-error empty.
	p, err := Locate()
	if err == nil && p == "" {
		t.Error("Locate() returned (\"\", nil); want either a path or an error")
	}
}
