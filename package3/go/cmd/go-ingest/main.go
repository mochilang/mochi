// Command go-ingest reads a Go module's source tree and emits an
// ApiSurface JSON document describing its exported surface. The
// document is consumed by later phases of the MEP-74 bridge.
//
// Usage:
//
//	go-ingest -module <path> [-version <semver>] [-dir <dir>] [-output <file>] [pattern]
//
// If pattern is omitted, "./..." is used. If -output is "-" or
// omitted, the JSON is written to stdout. Exit code is non-zero on
// load or ingest failure.
package main

import (
	"flag"
	"fmt"
	"os"
	"strings"

	"golang.org/x/tools/go/packages"

	"mochi/package3/go/apisurface"
)

func main() {
	if err := run(os.Args[1:], os.Stdout, os.Stderr); err != nil {
		fmt.Fprintf(os.Stderr, "go-ingest: %v\n", err)
		os.Exit(1)
	}
}

func run(args []string, stdout, stderr *os.File) error {
	fs := flag.NewFlagSet("go-ingest", flag.ContinueOnError)
	fs.SetOutput(stderr)
	var (
		module      = fs.String("module", "", "canonical module path (required)")
		version     = fs.String("version", "", "resolved semver (optional)")
		dir         = fs.String("dir", ".", "directory to load packages from")
		output      = fs.String("output", "-", "output file path; \"-\" for stdout")
		generatedBy = fs.String("generated-by", "mochi-go-bridge go-ingest", "producer identifier")
	)
	if err := fs.Parse(args); err != nil {
		return err
	}
	if *module == "" {
		return fmt.Errorf("-module is required")
	}
	pattern := "./..."
	if rest := fs.Args(); len(rest) > 0 {
		pattern = rest[0]
	}

	cfg := &packages.Config{
		Mode: packages.NeedName | packages.NeedFiles | packages.NeedCompiledGoFiles |
			packages.NeedImports | packages.NeedTypes | packages.NeedSyntax |
			packages.NeedTypesInfo | packages.NeedDeps | packages.NeedModule,
		Dir:   *dir,
		Tests: false,
	}
	pkgs, err := packages.Load(cfg, pattern)
	if err != nil {
		return fmt.Errorf("packages.Load: %w", err)
	}
	var loadErrs []string
	for _, p := range pkgs {
		for _, e := range p.Errors {
			loadErrs = append(loadErrs, fmt.Sprintf("%s: %v", p.PkgPath, e))
		}
	}
	if len(loadErrs) > 0 {
		return fmt.Errorf("load errors:\n  %s", joinLines(loadErrs))
	}

	f, err := apisurface.Ingest(pkgs, apisurface.IngestOptions{
		Module:      *module,
		Version:     *version,
		GeneratedBy: *generatedBy,
	})
	if err != nil {
		return err
	}

	buf, err := f.Encode()
	if err != nil {
		return err
	}
	if *output == "-" || *output == "" {
		if _, err := stdout.Write(buf); err != nil {
			return err
		}
		return nil
	}
	return os.WriteFile(*output, buf, 0o644)
}

func joinLines(s []string) string {
	return strings.Join(s, "\n  ")
}
