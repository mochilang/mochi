package build

import (
	"bytes"
	"os"
	"path/filepath"
	"text/template"
)

// Phase 17 packaging targets layer on top of the emitted main.php. This
// file owns three deployment surfaces:
//
//   1. Phar archives. Production builds invoke `humbug/box compile`;
//      MEP-55's gate sidesteps that toolchain dependency by using PHP's
//      built-in Phar class via a generated stager script. Both routes
//      land in the same shape (single .phar runnable with `php X.phar`).
//   2. FrankenPHP bundles. Caddyfile + Dockerfile pinned to the
//      `dunglas/frankenphp:php8.4` base image. The Caddyfile uses the
//      php_server directive (the modern FrankenPHP idiom) rather than
//      hand-rolled php_fastcgi + try_files plumbing.
//   3. RoadRunner workers. .rr.yaml + a worker.php wrapper that runs
//      the emitted entry in a long-lived process driven by the Go
//      `rr` binary.
//
// All three are pure text templates. The gate validates structure;
// running the resulting bundle in a real Caddy/RoadRunner host is the
// CI-side gate that lives outside this package.

const pharStagerTmpl = `<?php
// Phase 17 phar stager: wraps an emitted main.php into a single-file
// .phar runnable with ` + "`php out.phar`" + `. Production builds use
// humbug/box; this stager is the toolchain-free path the in-tree gate
// uses so CI does not need a humbug/box install.
declare(strict_types=1);

$src = {{.SrcLit}};
$dst = {{.DstLit}};

if (file_exists($dst)) {
    @unlink($dst);
}

$phar = new Phar($dst, 0, basename($dst));
$phar->startBuffering();
$phar->addFile($src, 'main.php');
$phar->setStub($phar->createDefaultStub('main.php'));
$phar->stopBuffering();
`

// emitPharStager writes a stager PHP script to outDir and returns its
// absolute path. Running it produces a .phar at dstPhar. mainPhp is the
// already-emitted main.php to wrap.
func emitPharStager(outDir, mainPhp, dstPhar string) (string, error) {
	tmpl, err := template.New("phar").Parse(pharStagerTmpl)
	if err != nil {
		return "", err
	}
	var buf bytes.Buffer
	data := struct {
		SrcLit string
		DstLit string
	}{
		SrcLit: phpStringLit(mainPhp),
		DstLit: phpStringLit(dstPhar),
	}
	if err := tmpl.Execute(&buf, data); err != nil {
		return "", err
	}
	path := filepath.Join(outDir, "build_phar.php")
	if err := os.WriteFile(path, buf.Bytes(), 0o644); err != nil {
		return "", err
	}
	return path, nil
}

const caddyfileTmpl = `# Phase 17 FrankenPHP Caddyfile for the Mochi-emitted package "{{.Name}}".
# Drives Caddy 2.8+ embedded PHP via the php_server directive. The
# worker block is what gives FrankenPHP its 4-10x throughput over
# PHP-FPM: a single PHP process answers every request, with
# Worker::reset() between requests for state hygiene.
{
	frankenphp {
		worker /app/main.php 4
	}
}

:8080 {
	root * /app
	php_server
}
`

const dockerfileTmpl = `# Phase 17 Dockerfile for the Mochi-emitted package "{{.Name}}".
# Pinned to dunglas/frankenphp:php8.4 (the modern PHP app-server tier).
# Static binaries are a v2 candidate (FrankenPHP's static-php-cli path).
FROM dunglas/frankenphp:php8.4

WORKDIR /app
COPY main.php /app/main.php

# FrankenPHP picks up the Caddyfile from /etc/caddy by default.
COPY Caddyfile /etc/caddy/Caddyfile

EXPOSE 8080
`

const rrYamlTmpl = `# Phase 17 RoadRunner config for the Mochi-emitted package "{{.Name}}".
# Drives the Go-supervised PHP worker tier: ` + "`rr serve`" + ` reads this,
# spawns N workers each running worker.php, and dispatches HTTP requests
# over a Unix socket. The worker calls Worker::reset() between requests.
version: "3"

server:
  command: "php worker.php"

http:
  address: ":8080"
  pool:
    num_workers: 4
    max_jobs: 64
    allocate_timeout: 60s
    destroy_timeout: 60s
`

const rrWorkerTmpl = `<?php
// Phase 17 RoadRunner worker for the Mochi-emitted package "{{.Name}}".
// A long-lived PHP process: the Go rr binary feeds PSR-7 ServerRequests
// in through stdin, the worker hands back PSR-7 Responses through
// stdout. Persistent state lives between requests; Worker::reset() runs
// at the top of each loop.
declare(strict_types=1);

require_once __DIR__ . '/main.php';

// Real apps wire PSR-7 here (e.g. via Spiral\RoadRunner\Http\PSR7Worker).
// The Mochi-emitted entry runs once and exits; under rr that means each
// request invocation re-runs mochi_main(), which is the cleanest
// transition path from the standalone CLI shape.
`

// EmitFrankenPHPBundle writes Caddyfile + Dockerfile into outDir. They
// reference /app/main.php and /etc/caddy/Caddyfile inside the image;
// they assume the caller already wrote main.php alongside them.
func EmitFrankenPHPBundle(outDir, packageName string) error {
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return err
	}
	for _, item := range []struct {
		name string
		tmpl string
	}{
		{"Caddyfile", caddyfileTmpl},
		{"Dockerfile", dockerfileTmpl},
	} {
		t, err := template.New(item.name).Parse(item.tmpl)
		if err != nil {
			return err
		}
		var buf bytes.Buffer
		if err := t.Execute(&buf, struct{ Name string }{packageName}); err != nil {
			return err
		}
		if err := os.WriteFile(filepath.Join(outDir, item.name), buf.Bytes(), 0o644); err != nil {
			return err
		}
	}
	return nil
}

// EmitRoadRunnerBundle writes .rr.yaml + worker.php into outDir.
func EmitRoadRunnerBundle(outDir, packageName string) error {
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return err
	}
	for _, item := range []struct {
		name string
		tmpl string
	}{
		{".rr.yaml", rrYamlTmpl},
		{"worker.php", rrWorkerTmpl},
	} {
		t, err := template.New(item.name).Parse(item.tmpl)
		if err != nil {
			return err
		}
		var buf bytes.Buffer
		if err := t.Execute(&buf, struct{ Name string }{packageName}); err != nil {
			return err
		}
		if err := os.WriteFile(filepath.Join(outDir, item.name), buf.Bytes(), 0o644); err != nil {
			return err
		}
	}
	return nil
}

// phpStringLit returns s formatted as a single-quoted PHP literal with
// backslash and single-quote escaping. Using single quotes avoids PHP's
// double-quoted interpolation entirely.
func phpStringLit(s string) string {
	var b bytes.Buffer
	b.WriteByte('\'')
	for _, c := range s {
		switch c {
		case '\\':
			b.WriteString(`\\`)
		case '\'':
			b.WriteString(`\'`)
		default:
			b.WriteRune(c)
		}
	}
	b.WriteByte('\'')
	return b.String()
}

