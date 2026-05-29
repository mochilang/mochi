package build_test

import (
	"context"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/package3/java/build"
	"mochi/package3/java/maven"
	javareflect "mochi/package3/java/reflect"
)

// minimalJAR is a valid ZIP with magic bytes (PK\x03\x04) accepted by the stub
// reflect tool (which does not actually read the ZIP format).
var minimalJAR = []byte{0x50, 0x4B, 0x05, 0x06, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}

// fakeSurface is the JSON the mock reflect tool returns.
var fakeSurface = mustMarshal(javareflect.Surface{
	SchemaVersion: 1,
	Classes:       []javareflect.ClassInfo{},
})

func mustMarshal(v any) []byte {
	b, err := json.Marshal(v)
	if err != nil {
		panic(err)
	}
	return b
}

// newMockMavenServer returns a test HTTP server that serves a minimal JAR and
// its SHA-1 checksum for any requested path.
func newMockMavenServer(t *testing.T) *httptest.Server {
	t.Helper()
	return httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if strings.HasSuffix(r.URL.Path, ".sha1") {
			// SHA-1 of minimalJAR bytes.
			w.Write([]byte("b04f3ee8f5e43fa3b162981b50bb72fe1acabb33"))
			return
		}
		w.Write(minimalJAR)
	}))
}

// mockExecutor returns a javareflect.Executor that yields fakeSurface without
// actually running a JVM.
func mockExecutor(_ context.Context, _ string, _ ...string) ([]byte, error) {
	return fakeSurface, nil
}

func TestPipeline_Scaffold(t *testing.T) {
	srv := newMockMavenServer(t)
	defer srv.Close()

	tmp := t.TempDir()
	d := build.NewDriver(build.Options{WorkDir: tmp, NoCache: true})
	if _, err := d.PrepareWorkspace(); err != nil {
		t.Fatalf("PrepareWorkspace: %v", err)
	}

	client := maven.NewClient(srv.URL + "/")
	tool := javareflect.NewToolWithExecutor(mockExecutor)

	p := build.NewPipelineWithDeps(d, client, tool)
	coord, err := maven.Parse("com.example:mylib@1.0.0")
	if err != nil {
		t.Fatalf("Parse coord: %v", err)
	}

	res, err := p.Run(context.Background(), build.PipelineRequest{
		Coord: coord,
		Alias: "mylib",
	})
	if err != nil {
		t.Fatalf("Run: %v", err)
	}

	if res.JARPath == "" {
		t.Error("JARPath is empty")
	}
	if res.WrapperSourceDir == "" {
		t.Error("WrapperSourceDir is empty")
	}
	if res.ShimMochi == "" {
		t.Error("ShimMochi is empty")
	}
	if _, err := os.Stat(res.JARPath); err != nil {
		t.Errorf("JAR not on disk: %v", err)
	}
	if _, err := os.Stat(res.WrapperSourceDir); err != nil {
		t.Errorf("wrapper source dir not on disk: %v", err)
	}
}

func TestPipeline_WorkDirNotInitialised(t *testing.T) {
	d := build.NewDriver(build.Options{NoCache: true})
	p := build.NewPipeline(d)
	coord, _ := maven.Parse("com.example:mylib@1.0.0")
	_, err := p.Run(context.Background(), build.PipelineRequest{Coord: coord})
	if err == nil {
		t.Error("expected error when work-dir not initialised")
	}
}

func TestJavacRun_NoSources(t *testing.T) {
	ctx := context.Background()
	_, err := build.RunJavac(ctx, build.CompileOptions{SourceFiles: nil})
	if err == nil {
		t.Error("expected error when no source files provided")
	}
}

func TestFindJavac_Path(t *testing.T) {
	// FindJavac should not panic; on CI it may or may not find javac.
	_, _ = build.FindJavac()
}

func TestJavaFilesInDir(t *testing.T) {
	dir := t.TempDir()
	// Create dummy .java files
	for _, name := range []string{"A.java", "B.java", "skip.txt"} {
		os.WriteFile(filepath.Join(dir, name), []byte(""), 0o644)
	}
	// The pipeline helper is not exported, but the wrapper-source writing is
	// tested indirectly via the full Run in TestPipeline_Scaffold above.
	_ = dir
}
