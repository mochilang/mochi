package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase17Phar gates the Phar packaging surface. For each fixture:
//
//   1. Lower the .mochi through the PHP transpiler to main.php.
//   2. Generate a stager PHP script that wraps main.php into a .phar
//      using PHP's built-in Phar class (no humbug/box dependency).
//   3. Run the stager under `php -d phar.readonly=0` to produce out.phar.
//   4. Run `php out.phar` and diff stdout against the .out cassette.
//
// Skips when PHP is not on PATH; CI uses shivammathur/setup-php@v2 with
// `tools: composer:v2` to drive this gate. Production builds use
// `humbug/box compile` which produces a richer Phar (compaction,
// compression, GPG-signable); the in-tree gate just validates the
// runnable-archive shape so the rest of the pipeline can trust it.
func TestPhase17Phar(t *testing.T) {
	if _, err := exec.LookPath("php"); err != nil {
		if p := os.Getenv("PHP_PATH"); p == "" {
			t.Skipf("php not on PATH: %v", err)
		}
	}

	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase17-packaging")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}

	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		name := strings.TrimSuffix(e.Name(), ".mochi")
		t.Run(name, func(t *testing.T) {
			runPharFixture(t,
				filepath.Join(fixtureDir, e.Name()),
				filepath.Join(fixtureDir, name+".out"))
		})
	}
}

func runPharFixture(t *testing.T, mochiPath, wantFile string) {
	t.Helper()

	want, err := os.ReadFile(wantFile)
	if err != nil {
		t.Fatalf("read want %s: %v", wantFile, err)
	}

	outDir := t.TempDir()
	d := &Driver{CacheDir: t.TempDir(), NoCache: true}
	mainPhp, err := d.Build(mochiPath, outDir, TargetPhpSource)
	if err != nil {
		t.Fatalf("Build(%s): %v", filepath.Base(mochiPath), err)
	}

	pharPath := filepath.Join(outDir, "out.phar")
	stagerPath, err := emitPharStager(outDir, mainPhp, pharPath)
	if err != nil {
		t.Fatalf("emitPharStager: %v", err)
	}

	// phar.readonly defaults to 1 on most distributions; the stager
	// needs write access for Phar::startBuffering + addFile.
	stageCmd := exec.Command("php", "-d", "phar.readonly=0", stagerPath)
	stageCmd.Stderr = os.Stderr
	if err := stageCmd.Run(); err != nil {
		t.Fatalf("phar stager: %v", err)
	}
	if _, err := os.Stat(pharPath); err != nil {
		t.Fatalf("phar not produced at %s: %v", pharPath, err)
	}

	runCmd := exec.Command("php", pharPath)
	var stdout bytes.Buffer
	runCmd.Stdout = &stdout
	runCmd.Stderr = os.Stderr
	if err := runCmd.Run(); err != nil {
		t.Fatalf("run phar: %v", err)
	}
	if !bytes.Equal(stdout.Bytes(), want) {
		t.Errorf("phar stdout mismatch\ngot:  %q\nwant: %q", stdout.Bytes(), want)
	}
}

// TestPhase17FrankenPHPBundle gates the FrankenPHP packaging surface:
// Caddyfile + Dockerfile are emitted with the structural shape an app
// server deployment needs. We do not run Docker or boot Caddy in this
// gate; the CI pipeline does the end-to-end run.
//
// Structural assertions match the spec's §12 promises: php_server
// directive (the modern FrankenPHP idiom), worker block (the 4-10x
// throughput path), and base image pinned to dunglas/frankenphp:php8.4.
func TestPhase17FrankenPHPBundle(t *testing.T) {
	outDir := t.TempDir()
	if err := EmitFrankenPHPBundle(outDir, "pkg_hello"); err != nil {
		t.Fatalf("EmitFrankenPHPBundle: %v", err)
	}

	caddyfile, err := os.ReadFile(filepath.Join(outDir, "Caddyfile"))
	if err != nil {
		t.Fatalf("read Caddyfile: %v", err)
	}
	for _, want := range []string{
		"frankenphp {",
		// Pin the worker count suffix ("4") on the worker line; the
		// spec calls out a 4-process default for the throughput tier,
		// and the bare `worker /app/main.php` form would still match
		// a regression that dropped the count.
		"worker /app/main.php 4",
		"php_server",
		"root * /app",
		":8080",
		"pkg_hello",
	} {
		if !strings.Contains(string(caddyfile), want) {
			t.Errorf("Caddyfile missing %q\n---\n%s", want, caddyfile)
		}
	}

	dockerfile, err := os.ReadFile(filepath.Join(outDir, "Dockerfile"))
	if err != nil {
		t.Fatalf("read Dockerfile: %v", err)
	}
	for _, want := range []string{
		"FROM dunglas/frankenphp:php8.4",
		"WORKDIR /app",
		"COPY main.php /app/main.php",
		"COPY Caddyfile /etc/caddy/Caddyfile",
		"EXPOSE 8080",
	} {
		if !strings.Contains(string(dockerfile), want) {
			t.Errorf("Dockerfile missing %q\n---\n%s", want, dockerfile)
		}
	}
}

// TestPhase17RoadRunnerBundle gates the RoadRunner packaging surface:
// .rr.yaml + worker.php are emitted with the structural shape the Go
// rr binary needs to spawn workers.
//
// The worker count and pool sizing mirror the spec's defaults (4
// workers, 64 max-jobs). Worker::reset() is referenced in the worker
// comment; the actual reset call lives in the runtime per the spec's
// §C2 worker-mode guidance.
func TestPhase17RoadRunnerBundle(t *testing.T) {
	outDir := t.TempDir()
	if err := EmitRoadRunnerBundle(outDir, "pkg_hello"); err != nil {
		t.Fatalf("EmitRoadRunnerBundle: %v", err)
	}

	rrYaml, err := os.ReadFile(filepath.Join(outDir, ".rr.yaml"))
	if err != nil {
		t.Fatalf("read .rr.yaml: %v", err)
	}
	for _, want := range []string{
		`version: "3"`,
		`command: "php worker.php"`,
		"http:",
		// The spec calls out 4 workers + 64 max-jobs as the defaults;
		// the timeouts come from the same template block and would
		// silently disappear without a gate. Pin all four so a
		// regression that dropped any one fails the test.
		`address: ":8080"`,
		"num_workers: 4",
		"max_jobs: 64",
		"allocate_timeout: 60s",
		"destroy_timeout: 60s",
		"pkg_hello",
	} {
		if !strings.Contains(string(rrYaml), want) {
			t.Errorf(".rr.yaml missing %q\n---\n%s", want, rrYaml)
		}
	}

	worker, err := os.ReadFile(filepath.Join(outDir, "worker.php"))
	if err != nil {
		t.Fatalf("read worker.php: %v", err)
	}
	for _, want := range []string{
		"<?php",
		"declare(strict_types=1);",
		"require_once __DIR__ . '/main.php';",
		"pkg_hello",
	} {
		if !strings.Contains(string(worker), want) {
			t.Errorf("worker.php missing %q\n---\n%s", want, worker)
		}
	}
}

// TestPhase17PharRoundTripsAllFixtures is the cross-cut gate: every
// Phase 17 fixture builds a runnable phar AND emits FrankenPHP /
// RoadRunner bundles alongside, all in one go. Mirrors the Phase 15
// LowersAllFeatures pattern but layered with the three deployment
// targets active at once.
func TestPhase17AllTargetsTogether(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase17-packaging")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		name := strings.TrimSuffix(e.Name(), ".mochi")
		t.Run(name, func(t *testing.T) {
			outDir := t.TempDir()
			d := &Driver{CacheDir: t.TempDir(), NoCache: true}
			mainPhp, err := d.Build(filepath.Join(fixtureDir, e.Name()), outDir, TargetPhpSource)
			if err != nil {
				t.Fatalf("Build(%s): %v", name, err)
			}
			if _, err := os.Stat(mainPhp); err != nil {
				t.Fatalf("main.php missing: %v", err)
			}

			pharPath := filepath.Join(outDir, name+".phar")
			if _, err := emitPharStager(outDir, mainPhp, pharPath); err != nil {
				t.Fatalf("emitPharStager: %v", err)
			}
			if err := EmitFrankenPHPBundle(outDir, name); err != nil {
				t.Fatalf("EmitFrankenPHPBundle: %v", err)
			}
			if err := EmitRoadRunnerBundle(outDir, name); err != nil {
				t.Fatalf("EmitRoadRunnerBundle: %v", err)
			}

			for _, want := range []string{"build_phar.php", "Caddyfile", "Dockerfile", ".rr.yaml", "worker.php"} {
				if _, err := os.Stat(filepath.Join(outDir, want)); err != nil {
					t.Errorf("expected artifact %s missing: %v", want, err)
				}
			}
		})
	}
}
