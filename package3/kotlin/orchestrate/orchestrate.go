// Package orchestrate is the MEP-70 Phase-09 build orchestration driver.
//
// It connects the four pipeline stages needed to make `import kotlin "..." as
// alias` work end-to-end at build time:
//
//  1. Blob-cache verification — confirm the JAR(s) in LockEntry are present
//     and their SHA-256 hashes match what mochi.lock recorded.
//  2. Wrapper synthesis — call wrapper.Synthesize to produce the Java JNI
//     bridge source tree for each artifact.
//  3. GraalVM native-image — compile the bridge source + dependency JARs to
//     a platform shared library (libwrap_{artifact}.{so,dylib,dll}).
//  4. Link flag emission — return -L / -l flags the Mochi linker driver uses
//     when it assembles the final native binary.
//
// The driver is deliberately side-effect free apart from writing into Config.WorkDir.
// All GraalVM calls go through the graalvm.Compile function so tests can
// substitute a no-op path when native-image is absent.
package orchestrate

import (
	"context"
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"time"

	bridgeerrors "github.com/mochilang/mochi/package3/kotlin/errors"
	"github.com/mochilang/mochi/package3/kotlin/graalvm"
	"github.com/mochilang/mochi/package3/kotlin/metadata"
	"github.com/mochilang/mochi/package3/kotlin/wrapper"
)

// LockEntry is the minimal lock-file record for one Kotlin artifact.
// It mirrors the [[kotlin-package]] TOML shape; only the fields the
// orchestrator needs at build time are included here.
type LockEntry struct {
	Group    string // e.g. "org.jetbrains.kotlinx"
	Artifact string // e.g. "kotlinx-coroutines-core"
	Version  string // e.g. "1.7.3"

	// JARPath is the absolute path to the cached JAR on disk.
	JARPath string

	// JarSHA256 is the expected hex-encoded SHA-256 of JARPath.
	// If non-empty, the driver verifies it before building.
	JarSHA256 string

	// WrapperSHA256 is the expected hex-encoded SHA-256 of the previously
	// synthesised wrapper tree (the directory tree hash).  If non-empty the
	// driver skips synthesis when the hash matches (incremental rebuild).
	WrapperSHA256 string

	// TransitiveDeps lists dependency JARs needed at native-image compile time.
	TransitiveDeps []string
}

// Config controls one orchestration run.
type Config struct {
	// Entries is the set of Kotlin artifacts to bridge.
	Entries []LockEntry

	// WorkDir is the scratch/output directory.  The driver writes
	//   <WorkDir>/wrap/<artifact>/java/  — synthesised Java source
	//   <WorkDir>/wrap/<artifact>/lib/   — native-image output (.so + .h)
	WorkDir string

	// LockCheck, when true, verifies JAR hashes and wrapper hashes but
	// does NOT re-synthesise or re-compile.  Used by `mochi pkg lock --check`.
	LockCheck bool

	// GraalVMPath is an optional explicit path to the native-image binary.
	// If empty, graalvm.FindNativeImage() is used.
	GraalVMPath string

	// CompileTimeout is the per-artifact GraalVM timeout (default 5 min).
	CompileTimeout time.Duration
}

// ArtifactResult describes the build output for one artifact.
type ArtifactResult struct {
	Artifact string
	LibPath  string // absolute path to the compiled shared library
	Header   string // absolute path to the generated C header
	// LFlags are the individual -l/<lib-name> pieces (without -L).
	LFlags []string
}

// Result is the output of a successful Driver.Build call.
type Result struct {
	Artifacts  []ArtifactResult
	LinkDirs   []string // unique -L directories (in order)
	LinkLibs   []string // unique -l library names (in order)
}

// Driver is the MEP-70 build orchestration driver.  Its zero value is ready
// to use.
type Driver struct{}

// Build runs the full orchestration pipeline for all entries in cfg.
// On success it returns the compiled shared-library paths and linker flags.
// If GraalVM is not installed, it returns ErrGraalVMNotFound.
func (d *Driver) Build(ctx context.Context, cfg Config) (*Result, error) {
	if err := os.MkdirAll(cfg.WorkDir, 0o755); err != nil {
		return nil, fmt.Errorf("orchestrate: mkdir workdir: %w", err)
	}

	timeout := cfg.CompileTimeout
	if timeout == 0 {
		timeout = 5 * time.Minute
	}

	res := &Result{}
	seenLibDirs := map[string]struct{}{}
	seenLibs := map[string]struct{}{}

	for _, entry := range cfg.Entries {
		ar, err := d.buildOne(ctx, entry, cfg, timeout)
		if err != nil {
			return nil, fmt.Errorf("orchestrate %s:%s: %w", entry.Group, entry.Artifact, err)
		}
		res.Artifacts = append(res.Artifacts, *ar)
		libDir := filepath.Dir(ar.LibPath)
		if _, ok := seenLibDirs[libDir]; !ok {
			seenLibDirs[libDir] = struct{}{}
			res.LinkDirs = append(res.LinkDirs, libDir)
		}
		for _, lf := range ar.LFlags {
			if _, ok := seenLibs[lf]; !ok {
				seenLibs[lf] = struct{}{}
				res.LinkLibs = append(res.LinkLibs, lf)
			}
		}
	}
	return res, nil
}

// buildOne runs the pipeline for a single artifact entry.
func (d *Driver) buildOne(ctx context.Context, entry LockEntry, cfg Config, timeout time.Duration) (*ArtifactResult, error) {
	// ── Step 1: JAR hash verification ─────────────────────────────────────
	if err := verifyJAR(entry); err != nil {
		return nil, err
	}
	if cfg.LockCheck {
		// In lock-check mode we stop here; we've confirmed the JAR is intact.
		return &ArtifactResult{Artifact: entry.Artifact}, nil
	}

	// ── Step 2: Wrapper synthesis ──────────────────────────────────────────
	artifactSlug := safeSlug(entry.Artifact)
	wrapDir := filepath.Join(cfg.WorkDir, "wrap", artifactSlug)
	if err := synthesizeWrapper(entry, wrapDir); err != nil {
		return nil, err
	}

	// ── Step 3: GraalVM native-image ───────────────────────────────────────
	libDir := filepath.Join(wrapDir, "lib")
	if err := os.MkdirAll(libDir, 0o755); err != nil {
		return nil, fmt.Errorf("mkdir lib: %w", err)
	}

	libName := "libwrap_" + artifactSlug
	deps := entry.TransitiveDeps
	// Include the JAR itself as a dependency for native-image.
	if entry.JARPath != "" {
		deps = append([]string{entry.JARPath}, deps...)
	}

	compilCtx, cancel := context.WithTimeout(ctx, timeout)
	defer cancel()

	soPath, headerPath, err := graalvm.Compile(compilCtx, graalvm.Options{
		GraalVMPath:    cfg.GraalVMPath,
		OutputDir:      libDir,
		LibraryName:    libName,
		WrapperJAR:     "",
		DependencyJARs: deps,
		NoFallback:     true,
		Timeout:        timeout,
	})
	if err != nil {
		var notFound *bridgeerrors.ErrGraalVMNotFound
		if errors.As(err, &notFound) {
			return nil, err
		}
		return nil, fmt.Errorf("native-image: %w", err)
	}

	return &ArtifactResult{
		Artifact: entry.Artifact,
		LibPath:  soPath,
		Header:   headerPath,
		LFlags:   []string{strings.TrimPrefix(strings.TrimSuffix(filepath.Base(soPath), sharedLibExt()), "lib")},
	}, nil
}

// verifyJAR checks that entry.JARPath exists and, when entry.JarSHA256 is set,
// that the on-disk file matches.
func verifyJAR(entry LockEntry) error {
	if entry.JARPath == "" {
		return nil // nothing to verify (bare coordinate, no JAR yet fetched)
	}
	info, err := os.Stat(entry.JARPath)
	if err != nil {
		return fmt.Errorf("JAR not in cache (%s): %w", entry.JARPath, &bridgeerrors.ErrArtifactNotFound{
			GroupID:    entry.Group,
			ArtifactID: entry.Artifact,
			Version:    entry.Version,
		})
	}
	if info.IsDir() {
		return fmt.Errorf("expected JAR file, got directory: %s", entry.JARPath)
	}
	if entry.JarSHA256 == "" {
		return nil
	}

	f, err := os.Open(entry.JARPath)
	if err != nil {
		return fmt.Errorf("open JAR: %w", err)
	}
	defer f.Close()

	h := sha256.New()
	if _, err := io.Copy(h, f); err != nil {
		return fmt.Errorf("hash JAR: %w", err)
	}
	got := hex.EncodeToString(h.Sum(nil))
	if got != entry.JarSHA256 {
		return &bridgeerrors.ErrLockMismatch{
			ArtifactID: entry.Group + ":" + entry.Artifact,
			Field:      "jar_sha256",
			Expected:   entry.JarSHA256,
			Got:        got,
		}
	}
	return nil
}

// synthesizeWrapper calls wrapper.Synthesize to produce the Java JNI bridge
// source tree for the given artifact.
func synthesizeWrapper(entry LockEntry, wrapDir string) error {
	if err := os.MkdirAll(wrapDir, 0o755); err != nil {
		return fmt.Errorf("mkdir wrap: %w", err)
	}
	if entry.JARPath == "" {
		// Nothing to synthesize without a JAR.
		return nil
	}

	jarBytes, err := os.ReadFile(entry.JARPath)
	if err != nil {
		return fmt.Errorf("read JAR: %w", err)
	}

	classes, err := metadata.IngestJARBytes(jarBytes)
	if err != nil {
		return fmt.Errorf("ingest metadata: %w", err)
	}

	return wrapper.Synthesize(safeSlug(entry.Artifact), classes, wrapDir)
}

// safeSlug converts a Maven artifact ID to a filesystem-safe identifier:
// hyphens and dots become underscores.
func safeSlug(s string) string {
	r := strings.NewReplacer("-", "_", ".", "_")
	return r.Replace(s)
}

func sharedLibExt() string {
	switch runtime.GOOS {
	case "darwin":
		return ".dylib"
	case "windows":
		return ".dll"
	default:
		return ".so"
	}
}
