---
title: "PHP target portability: CI matrix, phar.readonly, FrankenPHP, RoadRunner, Packagist"
description: "PHP ecosystem portability considerations: CI matrix (8.4.0/8.4/8.5), phar.readonly=0 requirement, FrankenPHP vs RoadRunner deployment, Composer 2.4+, Packagist, and server model implications for agent/stream design."
sidebar_position: 7
---

# PHP target portability: CI matrix, phar.readonly, FrankenPHP vs RoadRunner, Composer, Packagist

Author: research pass for MEP-55 (Mochi-to-PHP 8.4 transpiler).
Date: 2026-05-29 15:00 (GMT+7).
Sources: `.github/workflows/transpiler3-php-test.yml`,
`transpiler3/php/build/packaging.go`,
`transpiler3/php/build/phase17_test.go`,
`transpiler3/php/runtime/composer.json`,
`website/docs/mep/mep-0055.md`.

## 1. PHP version matrix in CI

The workflow `.github/workflows/transpiler3-php-test.yml` defines two jobs:

### 1.1 `go-side` job

Runs `go vet`, `go build`, and `go test` for the PHP transpiler packages
on `ubuntu-latest`. Uses PHP 8.4 (single version) installed via
`shivammathur/setup-php@v2` with `extensions: mbstring, gmp` and
`tools: composer:v2`. This job covers the Go-side compilation and all
fragment tests that do not require running PHP.

### 1.2 `php-runtime` job

Uses a matrix of three PHP versions:

| PHP version | `allow_failure` | Purpose |
|-------------|-----------------|---------|
| `8.4.0` | false | Pins the exact release the spec targets |
| `8.4` | false | Latest stable 8.4.x patch; must stay green |
| `8.5` | true | Forward-compat smoke; allowed to fail |

The `allow_failure: true` entry for 8.5 uses `continue-on-error: ${{
matrix.allow_failure }}` on each step. This means the 8.5 run is visible
in CI but does not block merges.

Each matrix run executes: `composer install`, PHPStan, Psalm,
php-cs-fixer dry-run, and PHPUnit. All four gates run under every PHP
version in the matrix.

### 1.3 Why 8.4.0 specifically

PHP 8.4.0 was released November 2024. The `8.4` matrix entry tracks
the latest patch release (e.g., 8.4.7 at the time of writing). Pinning
`8.4.0` separately ensures the package compiles on the initial GA release,
not just the latest patch. Some distributions and cloud hosting providers
pin to the exact GA release for months before updating.

### 1.4 Why 8.5 as allow_failure

PHP 8.5 was in alpha/beta in May 2026. Running the full test suite
against it provides early warning of breakage (new strict-type rules,
deprecated extensions, changed function signatures) without blocking CI.
The `^8.4` constraint in `composer.json` does not exclude 8.5 on the
Composer side; the `allow_failure` is purely a CI-policy choice.

## 2. The `phar.readonly` requirement

PHP's `phar.readonly` INI directive defaults to `1` on most Linux
distributions (Debian, Ubuntu, Alpine) and on macOS Homebrew. When
`phar.readonly = 1`, any attempt to write to or create a Phar archive
throws a `PharException`. The Phase 17 Phar stager calls
`Phar::startBuffering()`, `Phar::addFile()`, and `Phar::stopBuffering()`,
all of which require write access.

The stager is invoked with `php -d phar.readonly=0` (phase17_test.go
line 74):
```go
stageCmd := exec.Command("php", "-d", "phar.readonly=0", stagerPath)
```

The `-d` flag overrides the INI setting for that single invocation without
modifying the system `php.ini`. The resulting `.phar` file can then be
run without the override (`php out.phar` with default settings) because
`phar.readonly` only restricts writing to Phars, not reading or executing.

Deployments that want to automate Phar building (e.g., in a Docker build
step) need to either pass `-d phar.readonly=0` or set `phar.readonly = Off`
in a project-level `php.ini`. Production builds using `humbug/box` handle
this automatically.

## 3. FrankenPHP vs RoadRunner

Phase 17 ships two deployment targets beyond plain Phar:

### 3.1 FrankenPHP

FrankenPHP embeds the Zend Engine inside a Caddy 2.x server. It provides
two execution modes:

- **Classic mode**: one PHP process per request (like PHP-FPM).
- **Worker mode**: a long-lived PHP process that handles requests
  sequentially, calling `Worker::reset()` between requests. The
  Caddyfile emitted by `EmitFrankenPHPBundle` uses the worker mode:
  ```
  frankenphp {
      worker /app/main.php 4
  }
  ```
  The `worker /app/main.php 4` line starts 4 worker processes.

The Dockerfile is pinned to `dunglas/frankenphp:php8.4` (packaging.go
line 96):
```
FROM dunglas/frankenphp:php8.4
```

The `php_server` directive (Caddyfile line 8) is the modern FrankenPHP
idiom. The emitted Caddyfile also includes `root * /app` and exposes
`:8080`.

The `TestPhase17FrankenPHPBundle` test (phase17_test.go lines 103-145)
pins these structural requirements with `strings.Contains` assertions
rather than running Docker or booting Caddy.

### 3.2 RoadRunner

RoadRunner is a Go-based application server that spawns PHP worker
processes and dispatches HTTP requests over a Unix socket. The emitted
`.rr.yaml` (packaging.go lines 107-123) specifies:
```yaml
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
```

The emitted `worker.php` does `require_once __DIR__ . '/main.php'` and
includes a comment about `Worker::reset()` (packaging.go lines 125-138).
In RoadRunner's model, the Go `rr` binary feeds PSR-7 requests through
stdin; the worker processes them and responds through stdout.

### 3.3 Server model and agents/streams

Both FrankenPHP and RoadRunner use a worker model where PHP code runs in
long-lived processes. This is significant for Mochi agents and streams:

- In standard PHP (new process per request), agent state is lost at
  request end. Agents only persist within a single script execution.
- In worker mode, a PHP process lives across multiple requests. Agent
  and stream state persists between requests unless `Worker::reset()` is
  called. MEP-55 Phase 9-10 fixtures are all single-script programs;
  multi-request agent persistence is a future concern.
- PHP's lack of preemptive scheduling means agents cannot block the
  worker thread waiting for an async event. All Phase 9-10 lowerings
  use synchronous emit-before-recv patterns specifically because of this.

See [[09-agent-streams]] for the agent/stream design and
[[12-risks-and-alternatives]] for the scheduling risk.

## 4. Composer 2 requirement

The CI matrix uses `tools: composer:v2` in `shivammathur/setup-php@v2`.
Composer 2 is required for:

- `composer audit`: security vulnerability checking (a Phase 18 gate).
- `composer install --no-plugins`: the `--no-plugins` flag is a Composer 2
  feature used in locked-down CI environments.
- Parallel dependency resolution (faster CI).

Composer 1.x would work for basic installation but lacks the audit command.
The `minimum-stability: stable` in `composer.json` also requires Composer
2's more precise stability handling.

## 5. Packagist and the publishing pipeline

Phase 18 covers GPG-signed releases and Sigstore attestation. The
publication workflow (`.github/workflows/transpiler3-php-publish.yml`) is
not examined in detail in this note, but the relevant portability points
are:

- Packagist requires a `composer.json` with a unique `name` (`mochi/runtime`).
- GPG-signed tags: the workflow uses `actions/attest-build-provenance@v1`
  for Sigstore OIDC attestation (GitHub Actions OIDC is GA since April 2024).
- Semantic versioning: Composer's `^8.4` constraint uses semver compatibility
  operators; patch releases (8.4.x) are compatible but new major versions
  (9.0) would not be accepted.

## 6. PHP extension availability on common platforms

The two required extensions (`ext-mbstring`, `ext-gmp`) are available on:

- **Ubuntu 24.04**: `apt-get install php8.4-mbstring php8.4-gmp`
- **Alpine Linux**: `apk add php84-mbstring php84-gmp`
- **macOS Homebrew**: `brew install php` (includes both by default)
- **Docker `dunglas/frankenphp:php8.4`**: both extensions pre-installed
  in the base image
- **Shared hosting**: GMP availability varies; most cPanel/Plesk hosts
  have it, but low-cost shared hosting may not. This is an acceptable
  limitation documented in the MEP spec.

The `shivammathur/setup-php@v2` action installs both via `extensions:
mbstring, gmp` in the CI workflow.

## 7. PHP_PATH environment variable

The build driver's `resolvePhp()` function (build.go lines 128-145)
checks `PHP_PATH` first:
1. If `PHP_PATH` is set and points to a directory, it appends `php`.
2. Falls back to well-known binary paths: `/usr/bin/php`,
   `/usr/local/bin/php`, `/opt/homebrew/bin/php`.
3. Falls back to `exec.LookPath("php")`.

CI sets `PHP_PATH` via `shivammathur/setup-php@v2`'s output. Local
development falls through to `LookPath`, which finds `php` if it is on
`$PATH`. Tests skip gracefully when PHP is not available:
```go
if _, err := exec.LookPath("php"); err != nil {
    if p := os.Getenv("PHP_PATH"); p == "" {
        t.Skipf("php not on PATH: %v", err)
    }
}
```
This pattern appears in `runPhpFixture`, `runPhpLLMFixture`,
`runPharFixture`, and the per-phase tests that invoke PHP directly.
