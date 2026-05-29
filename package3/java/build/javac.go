package build

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

// JavacResult holds the output of a javac compilation step.
type JavacResult struct {
	// ClassDir is the directory containing the compiled .class files.
	ClassDir string
	// Stdout and Stderr capture javac output.
	Stdout []byte
	Stderr []byte
}

// CompileOptions configures a single javac invocation.
type CompileOptions struct {
	// SourceFiles is the list of .java files to compile.
	SourceFiles []string
	// ClassPath is the list of JARs and class directories added to -classpath.
	ClassPath []string
	// SourceDir is an optional source directory; its subdirs are placed on -sourcepath.
	SourceDir string
	// OutDir is where .class files are written. A temp dir is used when empty.
	OutDir string
	// JavacPath overrides the javac executable. Defaults to "javac" (resolved via PATH).
	JavacPath string
	// ExtraFlags are appended verbatim to the javac command line.
	ExtraFlags []string
}

// RunJavac invokes javac with the given options and returns the result.
// The returned ClassDir is valid until the caller deletes it.
func RunJavac(ctx context.Context, opts CompileOptions) (*JavacResult, error) {
	if len(opts.SourceFiles) == 0 {
		return nil, fmt.Errorf("javac: no source files provided")
	}

	outDir := opts.OutDir
	if outDir == "" {
		tmp, err := os.MkdirTemp("", "mochi-javac-")
		if err != nil {
			return nil, fmt.Errorf("javac: create output dir: %w", err)
		}
		outDir = tmp
	} else {
		if err := os.MkdirAll(outDir, 0o755); err != nil {
			return nil, fmt.Errorf("javac: create output dir %s: %w", outDir, err)
		}
	}

	javac := opts.JavacPath
	if javac == "" {
		javac = "javac"
	}

	args := []string{"-d", outDir, "-encoding", "UTF-8"}

	if len(opts.ClassPath) > 0 {
		args = append(args, "-classpath", strings.Join(opts.ClassPath, string(filepath.ListSeparator)))
	}
	if opts.SourceDir != "" {
		args = append(args, "-sourcepath", opts.SourceDir)
	}
	args = append(args, opts.ExtraFlags...)
	args = append(args, opts.SourceFiles...)

	cmd := exec.CommandContext(ctx, javac, args...)
	out, err := cmd.CombinedOutput()

	res := &JavacResult{
		ClassDir: outDir,
		Stdout:   out,
	}
	if err != nil {
		return res, fmt.Errorf("javac: compilation failed: %w\n%s", err, out)
	}
	return res, nil
}

// FindJavac returns the absolute path of the javac executable. It searches
// the JAVA_HOME/bin directory first (if JAVA_HOME is set), then falls back
// to exec.LookPath.
func FindJavac() (string, error) {
	if jh := os.Getenv("JAVA_HOME"); jh != "" {
		candidate := filepath.Join(jh, "bin", "javac")
		if _, err := os.Stat(candidate); err == nil {
			return candidate, nil
		}
	}
	return exec.LookPath("javac")
}
