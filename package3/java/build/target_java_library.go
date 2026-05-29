package build

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	"mochi/package3/java/maven"
)

// TargetJavaLibrary represents a publishable Maven artifact produced by the
// MEP-67 build pipeline: the generated wrapper JAR paired with its POM.
type TargetJavaLibrary struct {
	// Coord identifies the upstream Maven library this wrapper targets.
	Coord maven.Coordinate
	// WrapperPOMSpec is the spec used to generate the wrapper POM.
	WrapperSpec POMSpec
	// JARPath is the path to the compiled wrapper JAR on disk.
	JARPath string
	// POMPath is the path to the rendered wrapper pom.xml on disk.
	POMPath string
}

// EmitTargetOptions configures EmitTargetJavaLibrary.
type EmitTargetOptions struct {
	// OutDir is the directory where JAR and POM are written.
	// A subdirectory named after the artifact is created inside OutDir.
	OutDir string
	// ClassDir is the directory of compiled wrapper .class files (from RunJavac).
	ClassDir string
	// Coord is the upstream Maven coordinate.
	Coord maven.Coordinate
	// JavacPath overrides the javac executable. Defaults to "javac".
	JavacPath string
	// JarToolPath overrides the jar tool executable. Defaults to "jar".
	JarToolPath string
}

// EmitTargetJavaLibrary writes the wrapper POM and packages the wrapper
// class files into a JAR, producing a TargetJavaLibrary.
//
// When ClassDir is empty or contains no .class files, the JAR step is
// skipped and JARPath is left empty (the POM is still written).
func EmitTargetJavaLibrary(ctx context.Context, opts EmitTargetOptions) (*TargetJavaLibrary, error) {
	spec := WrapperPOMSpec(opts.Coord)

	artDir := filepath.Join(opts.OutDir, spec.ArtifactID, spec.Version)
	if err := os.MkdirAll(artDir, 0o755); err != nil {
		return nil, fmt.Errorf("target: create output dir %s: %w", artDir, err)
	}

	// Write POM.
	pomPath := filepath.Join(artDir, spec.ArtifactID+"-"+spec.Version+".pom")
	pomXML := RenderPOM(spec)
	if err := os.WriteFile(pomPath, []byte(pomXML), 0o644); err != nil {
		return nil, fmt.Errorf("target: write pom.xml: %w", err)
	}

	tgt := &TargetJavaLibrary{
		Coord:       opts.Coord,
		WrapperSpec: spec,
		POMPath:     pomPath,
	}

	// Package class files into a JAR.
	if opts.ClassDir != "" {
		classes := classFilesInDir(opts.ClassDir)
		if len(classes) > 0 {
			jarPath := filepath.Join(artDir, spec.ArtifactID+"-"+spec.Version+".jar")
			if err := runJarTool(ctx, opts.JarToolPath, jarPath, opts.ClassDir); err != nil {
				return tgt, fmt.Errorf("target: create wrapper JAR: %w", err)
			}
			tgt.JARPath = jarPath
		}
	}

	return tgt, nil
}

// runJarTool invokes the JDK `jar` tool to package a directory into a JAR.
func runJarTool(ctx context.Context, jarTool, outputPath, classDir string) error {
	tool := jarTool
	if tool == "" {
		tool = "jar"
	}
	if jh := os.Getenv("JAVA_HOME"); jh != "" && tool == "jar" {
		candidate := filepath.Join(jh, "bin", "jar")
		if _, err := os.Stat(candidate); err == nil {
			tool = candidate
		}
	}

	// jar cf output.jar -C classDir .
	args := []string{"cf", outputPath, "-C", classDir, "."}
	cmd := exec.CommandContext(ctx, tool, args...)
	if out, err := cmd.CombinedOutput(); err != nil {
		return fmt.Errorf("jar cf failed: %w\n%s", err, out)
	}
	return nil
}

// classFilesInDir returns all .class files directly inside dir (non-recursive).
func classFilesInDir(dir string) []string {
	entries, err := os.ReadDir(dir)
	if err != nil {
		return nil
	}
	var files []string
	for _, e := range entries {
		if !e.IsDir() && filepath.Ext(e.Name()) == ".class" {
			files = append(files, filepath.Join(dir, e.Name()))
		}
	}
	return files
}
