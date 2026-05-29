package graalvm

import (
	"context"
	"fmt"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"time"

	bridgeerrors "github.com/mochilang/mochi/package3/kotlin/errors"
)

// Options for native-image compilation.
type Options struct {
	GraalVMPath    string        // path to native-image binary (from FindNativeImage if empty)
	OutputDir      string        // where to write libwrap.so + libwrap.h
	LibraryName    string        // e.g. "libwrap" (without .so/.dylib)
	WrapperJAR     string        // the compiled JNI wrapper JAR
	DependencyJARs []string      // transitive dependency JARs
	ReflectConfig  string        // path to reflect-config.json (optional)
	ResourceConfig string        // path to resource-config.json (optional)
	InitAtRuntime  []string      // --initialize-at-run-time=<class> entries
	NoFallback     bool          // --no-fallback (default true in Compile)
	Verbose        bool          // -H:+PrintAnalysisCallTree
	Timeout        time.Duration // default 5 minutes
}

// SharedLibExtension returns the platform-specific shared library extension.
func SharedLibExtension() string {
	switch runtime.GOOS {
	case "darwin":
		return ".dylib"
	case "windows":
		return ".dll"
	default:
		return ".so"
	}
}

// Compile runs native-image --shared to produce libwrap.{so,dylib,dll} + libwrap.h.
// It requires GraalVM to be installed (detected via FindNativeImage if Options.GraalVMPath is "").
// Returns ErrGraalVMNotFound if native-image is not available.
// Returns ErrNativeImageBuildFailed if the build exits non-zero.
func Compile(ctx context.Context, opts Options) (soPath, headerPath string, err error) {
	niPath := opts.GraalVMPath
	if niPath == "" {
		var findErr error
		niPath, _, findErr = FindNativeImage()
		if findErr != nil {
			return "", "", ErrGraalVMNotFound
		}
	} else {
		if !isExecutable(niPath) {
			return "", "", ErrGraalVMNotFound
		}
	}

	libName := opts.LibraryName
	if libName == "" {
		libName = "libwrap"
	}

	timeout := opts.Timeout
	if timeout == 0 {
		timeout = 5 * time.Minute
	}

	args := buildArgs(opts, libName)

	ctx, cancel := context.WithTimeout(ctx, timeout)
	defer cancel()

	cmd := exec.CommandContext(ctx, niPath, args...)
	var stderr strings.Builder
	cmd.Stderr = &stderr

	if runErr := cmd.Run(); runErr != nil {
		exitCode := -1
		if exitErr, ok := runErr.(*exec.ExitError); ok {
			exitCode = exitErr.ExitCode()
		}
		artifactID := filepath.Base(opts.WrapperJAR)
		return "", "", &bridgeerrors.ErrNativeImageBuildFailed{
			ArtifactID: artifactID,
			ExitCode:   exitCode,
			Stderr:     stderr.String(),
		}
	}

	ext := SharedLibExtension()
	soPath = filepath.Join(opts.OutputDir, libName+ext)
	headerPath = filepath.Join(opts.OutputDir, libName+".h")
	return soPath, headerPath, nil
}

// buildArgs constructs the native-image command-line arguments from opts.
func buildArgs(opts Options, libName string) []string {
	var args []string

	args = append(args, "--shared")

	noFallback := opts.NoFallback
	if !noFallback {
		// default to true
		noFallback = true
	}
	if noFallback {
		args = append(args, "--no-fallback")
	}

	args = append(args, fmt.Sprintf("-H:Name=%s", libName))
	args = append(args, fmt.Sprintf("-H:Path=%s", opts.OutputDir))

	if opts.ReflectConfig != "" {
		args = append(args, fmt.Sprintf("-H:ReflectionConfigurationFiles=%s", opts.ReflectConfig))
	}

	if opts.ResourceConfig != "" {
		args = append(args, fmt.Sprintf("-H:ResourceConfigurationFiles=%s", opts.ResourceConfig))
	}

	if len(opts.InitAtRuntime) > 0 {
		args = append(args, fmt.Sprintf("--initialize-at-run-time=%s", strings.Join(opts.InitAtRuntime, ",")))
	}

	if opts.Verbose {
		args = append(args, "-H:+PrintAnalysisCallTree")
	}

	// Build classpath
	var cpParts []string
	if opts.WrapperJAR != "" {
		cpParts = append(cpParts, opts.WrapperJAR)
	}
	cpParts = append(cpParts, opts.DependencyJARs...)
	if len(cpParts) > 0 {
		sep := ":"
		if runtime.GOOS == "windows" {
			sep = ";"
		}
		args = append(args, "-cp", strings.Join(cpParts, sep))
	}

	args = append(args, "com.mochi.bridge.Main")

	return args
}
