package build

import (
	"context"
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"os"
	"path/filepath"

	javaerrors "mochi/package3/java/errors"
	"mochi/package3/java/emit"
	"mochi/package3/java/maven"
	javareflect "mochi/package3/java/reflect"
	"mochi/package3/java/wrapper"
)

// PipelineRequest describes a single Maven artifact to bridge.
type PipelineRequest struct {
	// Coord is the Maven coordinate of the upstream library.
	Coord maven.Coordinate
	// Alias is the Mochi-side namespace for the extern declarations.
	Alias string
}

// PipelineResult holds the artefacts produced for one Maven artifact.
type PipelineResult struct {
	// JARPath is the path to the downloaded (and cached) JAR.
	JARPath string
	// WrapperSourceDir is the directory containing the synthesised .java wrapper files.
	WrapperSourceDir string
	// ClassDir holds the compiled wrapper .class files (after RunJavac).
	ClassDir string
	// ShimMochi is the generated `extern java` Mochi source.
	ShimMochi string
	// Skips collects skip reports from the synthesiser for diagnostics.
	Skips []javaerrors.SkipReport
}

// Pipeline orchestrates the full MEP-67 build flow for a set of Java imports:
//
//  1. Fetch the JAR from Maven Central (or the content-addressed cache).
//  2. Run the reflection tool to extract the public API surface.
//  3. Synthesise JNI wrapper Java sources.
//  4. Emit the Mochi extern shim.
//  5. Compile the wrapper sources with javac.
type Pipeline struct {
	driver *Driver
	client *maven.Client
	tool   *javareflect.Tool
}

// NewPipeline constructs a Pipeline using the given Driver. The Maven HTTP
// client and reflect tool are initialised with their defaults.
func NewPipeline(d *Driver) *Pipeline {
	return &Pipeline{
		driver: d,
		client: maven.NewClient(""),
		tool:   javareflect.NewTool(),
	}
}

// NewPipelineWithDeps constructs a Pipeline with explicit dependencies. Used
// in tests to inject a mock Maven client and/or reflect tool executor.
func NewPipelineWithDeps(d *Driver, client *maven.Client, tool *javareflect.Tool) *Pipeline {
	return &Pipeline{driver: d, client: client, tool: tool}
}

// Run executes the full pipeline for a single PipelineRequest. It creates
// the work directory structure under Driver.WorkDir() automatically.
func (p *Pipeline) Run(ctx context.Context, req PipelineRequest) (*PipelineResult, error) {
	if p.driver.WorkDir() == "" {
		return nil, fmt.Errorf("pipeline: driver work-dir not initialised; call PrepareWorkspace first")
	}
	result := &PipelineResult{}

	// Step 1: resolve the JAR (cache or network).
	jarPath, err := p.resolveJAR(ctx, req.Coord)
	if err != nil {
		return nil, fmt.Errorf("pipeline: fetch JAR %s: %w", req.Coord, err)
	}
	result.JARPath = jarPath

	// Step 2: reflect the public surface.
	surface, err := p.tool.Invoke(ctx, jarPath)
	if err != nil {
		return nil, fmt.Errorf("pipeline: reflect %s: %w", req.Coord, err)
	}

	// Step 3: synthesise wrapper Java sources.
	srcDir, err := p.synthesiseWrappers(surface, req.Coord)
	if err != nil {
		return nil, fmt.Errorf("pipeline: synthesise wrappers: %w", err)
	}
	result.WrapperSourceDir = srcDir

	// Step 4: emit the Mochi extern shim and collect skip reports.
	var skips []javaerrors.SkipReport
	for _, cls := range surface.Classes {
		sr := wrapper.Synth(cls)
		skips = append(skips, emit.CollectSkips(cls, sr)...)
	}
	result.Skips = skips
	result.ShimMochi = emit.EmitShimMochi(surface, req.Coord)

	// Step 5: compile the wrapper sources.
	sources := javaFilesInDir(srcDir)
	if len(sources) > 0 {
		javacResult, err := RunJavac(ctx, CompileOptions{
			SourceFiles: sources,
			ClassPath:   []string{jarPath},
		})
		if err != nil {
			return result, fmt.Errorf("pipeline: javac: %w", err)
		}
		result.ClassDir = javacResult.ClassDir
	}

	return result, nil
}

// resolveJAR downloads the JAR from Maven Central (or reads it from the
// content-addressed cache). Returns the absolute path to the JAR on disk.
func (p *Pipeline) resolveJAR(ctx context.Context, coord maven.Coordinate) (string, error) {
	data, err := p.client.FetchJAR(ctx, coord)
	if err != nil {
		return "", err
	}

	if !p.driver.opts.NoCache && p.driver.CacheDir() != "" {
		cache := maven.NewCache(p.driver.CacheDir())
		sum := sha256sum(data)
		if err := cache.Put(sum, data); err != nil {
			return "", fmt.Errorf("cache put: %w", err)
		}
		return cache.Path(sum), nil
	}

	// No cache: write to work-dir.
	dest := filepath.Join(p.driver.WorkDir(), coord.JARFilename())
	if err := os.WriteFile(dest, data, 0o644); err != nil {
		return "", err
	}
	return dest, nil
}

// synthesiseWrappers generates one .java file per class in the surface and
// writes them to a new directory inside the work-dir. Returns the directory.
func (p *Pipeline) synthesiseWrappers(surface *javareflect.Surface, coord maven.Coordinate) (string, error) {
	srcDir := filepath.Join(p.driver.WorkDir(), "wrapper-src", coord.GroupID, coord.ArtifactID)
	if err := os.MkdirAll(srcDir, 0o755); err != nil {
		return "", err
	}
	for _, cls := range surface.Classes {
		sr := wrapper.Synth(cls)
		if sr == nil {
			continue
		}
		src := wrapper.EmitJavaSource(sr.WrapClass)
		outPath := filepath.Join(srcDir, sr.WrapClass.ClassName+".java")
		if err := os.WriteFile(outPath, []byte(src), 0o644); err != nil {
			return "", fmt.Errorf("write wrapper %s: %w", outPath, err)
		}
	}
	return srcDir, nil
}

// javaFilesInDir returns all *.java files directly inside dir (non-recursive).
func javaFilesInDir(dir string) []string {
	entries, err := os.ReadDir(dir)
	if err != nil {
		return nil
	}
	var files []string
	for _, e := range entries {
		if !e.IsDir() && filepath.Ext(e.Name()) == ".java" {
			files = append(files, filepath.Join(dir, e.Name()))
		}
	}
	return files
}

// sha256sum returns the lowercase hex SHA-256 of data.
func sha256sum(data []byte) string {
	h := sha256.Sum256(data)
	return hex.EncodeToString(h[:])
}
