// Package pharemit generates the PHP build script and Phar stub for
// distributing a Mochi-emitted PHP library as a self-contained .phar archive.
//
// A Phar (PHP Archive) bundles all library source files into a single
// distributable file. The emitter produces two PHP files:
//
//   - stub.php    - the Phar stub (bootstrap code embedded at the archive head)
//   - build.php   - the builder script: run with `php build.php` to produce
//     the .phar file from the library src/ tree
//
// The generated build.php uses PHP's built-in Phar class and requires
// phar.readonly = 0 in php.ini (or the -d flag). Compression is optional.
//
// Usage:
//
//	result := pharemit.Build(pharemit.Config{
//	    PharName:      "acme-mylib",
//	    PSR4Namespace: `Acme\MyLib`,
//	    SrcDir:        "src",
//	})
//	// result.Files["stub.php"]  - embed this as the Phar stub
//	// result.Files["build.php"] - run to create acme-mylib.phar
package pharemit

import (
	"fmt"
	"strings"
)

// Compression selects Phar compression algorithm.
type Compression int

const (
	// CompressNone leaves files uncompressed (default; no zlib/bz2 needed).
	CompressNone Compression = iota
	// CompressGZ uses zlib DEFLATE compression (requires zlib extension).
	CompressGZ
	// CompressBZ2 uses bzip2 compression (requires bz2 extension).
	CompressBZ2
)

// Config holds parameters for Phar generation.
type Config struct {
	// PharName is the base name without extension, e.g. "acme-mylib".
	// The output archive is PharName + ".phar".
	PharName string
	// PSR4Namespace is the PHP namespace root, e.g. "Acme\\MyLib".
	PSR4Namespace string
	// SrcDir is the source directory relative to the library root (default "src").
	SrcDir string
	// Compression selects the archive compression. Default: CompressNone.
	Compression Compression
	// StubPreamble is an optional PHP comment/banner to prepend to stub.php.
	StubPreamble string
}

// BuildResult holds the generated files.
type BuildResult struct {
	// Files maps relative file path -> PHP source content.
	Files map[string]string
	// PharFileName is the output archive name, e.g. "acme-mylib.phar".
	PharFileName string
}

// Build generates stub.php and build.php for the given config.
func Build(cfg Config) (*BuildResult, error) {
	if cfg.PharName == "" {
		return nil, fmt.Errorf("pharemit: PharName is required")
	}
	if cfg.PSR4Namespace == "" {
		return nil, fmt.Errorf("pharemit: PSR4Namespace is required")
	}
	srcDir := cfg.SrcDir
	if srcDir == "" {
		srcDir = "src"
	}

	pharFile := cfg.PharName + ".phar"
	result := &BuildResult{
		Files:        make(map[string]string),
		PharFileName: pharFile,
	}

	result.Files["stub.php"] = renderStub(cfg, pharFile)
	result.Files["build.php"] = renderBuild(cfg, pharFile, srcDir)

	return result, nil
}

func renderStub(cfg Config, pharFile string) string {
	ns := strings.TrimSuffix(cfg.PSR4Namespace, "\\")
	// Normalise namespace separator to forward slash for use in PHAR paths.
	nsPath := strings.ReplaceAll(ns, "\\", "/")
	var sb strings.Builder
	sb.WriteString("<?php\n")
	if cfg.StubPreamble != "" {
		sb.WriteString("// ")
		sb.WriteString(strings.ReplaceAll(cfg.StubPreamble, "\n", "\n// "))
		sb.WriteString("\n")
	}
	fmt.Fprintf(&sb, `
// Auto-generated Phar stub for %s
// Registers PSR-4 autoloader for namespace %s from within the archive.

Phar::mapPhar(%q);

spl_autoload_register(function (string $class) use (&$_pharFile): void {
    $ns = %q;
    $nsPath = %q;
    if (strncmp($class, $ns . '\\', strlen($ns) + 1) !== 0) {
        return;
    }
    $rel = substr($class, strlen($ns) + 1);
    $rel = str_replace('\\', '/', $rel);
    $path = 'phar://' . $_pharFile . '/src/' . $nsPath . '/' . $rel . '.php';
    if (file_exists($path)) {
        require $path;
    }
});

__HALT_COMPILER();
`, cfg.PharName, ns, pharFile, ns, nsPath) //nolint:errcheck
	return sb.String()
}

func renderBuild(cfg Config, pharFile, srcDir string) string {
	compressCall := ""
	switch cfg.Compression {
	case CompressGZ:
		compressCall = "\n$phar->compressFiles(Phar::GZ);"
	case CompressBZ2:
		compressCall = "\n$phar->compressFiles(Phar::BZ2);"
	}

	return fmt.Sprintf(`<?php
/**
 * Build script for %s
 *
 * Usage: php -d phar.readonly=0 build.php
 *
 * Requires: PHP 8.0+, phar.readonly=0
 */
declare(strict_types=1);

$pharFile = __DIR__ . '/' . %q;
$srcDir   = __DIR__ . '/' . %q;

if (file_exists($pharFile)) {
    unlink($pharFile);
}

$phar = new Phar($pharFile, 0, %q);
$phar->startBuffering();

// Add all PHP files from src/ preserving directory structure.
$iterator = new RecursiveIteratorIterator(
    new RecursiveDirectoryIterator($srcDir, FilesystemIterator::SKIP_DOTS)
);
foreach ($iterator as $file) {
    if ($file->getExtension() !== 'php') {
        continue;
    }
    $relative = substr((string) $file, strlen(__DIR__) + 1);
    $phar->addFile((string) $file, $relative);
}
%s
$phar->setStub(file_get_contents(__DIR__ . '/stub.php'));
$phar->stopBuffering();

echo "Built: $pharFile\n";
`, cfg.PharName, pharFile, srcDir, pharFile, compressCall)
}
