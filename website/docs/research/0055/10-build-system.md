---
title: "Build system: Driver.Build pipeline, resolvePhp, cacheKey, Phase 17 packaging, CI workflow"
description: "Driver.Build pipeline (TargetPhpSource vs TargetPhpRun), resolvePhp, effectiveCacheDir + cacheKey (reserved), Phase 17 packaging (emitPharStager, EmitFrankenPHPBundle, EmitRoadRunnerBundle), runtimeSourceDir + copyFile, and CI workflow."
sidebar_position: 10
---

# Build system: Driver.Build, resolvePhp, packaging, CI

Author: research pass for MEP-55 (Mochi-to-PHP 8.4 transpiler).
Date: 2026-05-29 15:00 (GMT+7).
Sources: `transpiler3/php/build/build.go`,
`transpiler3/php/build/packaging.go`,
`transpiler3/php/build/build_test.go`,
`transpiler3/php/build/phase17_test.go`,
`.github/workflows/transpiler3-php-test.yml`.

## 1. Driver struct

`Driver` (build.go lines 43-57) is the pipeline entry point:

```go
type Driver struct {
    CacheDir      string
    NoCache       bool
    Deterministic bool
    phpPath       string
}
```

- `CacheDir`: overrides `~/.cache/mochi/php/`. Not yet wired to actual
  caching; the field exists for forward compatibility.
- `NoCache`: reserved. Tests set `NoCache: true` for isolation.
- `Deterministic`: reserved. Today it is a no-op; `TestPhase16Non-
  DeterministicBuildsAlsoMatch` verifies the default path is also
  byte-reproducible.
- `phpPath`: cached result of `resolvePhp()`, set lazily on first
  `TargetPhpRun` build.

## 2. Target enum

```go
const (
    TargetPhpSource Target = iota
    TargetPhpRun
)
```

`TargetPhpSource` writes `main.php` to `outDir` and returns its path.
`TargetPhpRun` does the same, then invokes `php main.php` with stdout
and stderr forwarded to the caller's streams.

## 3. Driver.Build pipeline

`Build(src, outDir, target)` (build.go lines 64-123):

1. `os.ReadFile(src)` — reads the Mochi source bytes (stored in
   `srcBytes` for the future `cacheKey` integration).
2. `parser.Parse(src)` — shared Mochi parser.
3. `types.Check(ast, types.NewEnv(nil))` — shared type checker.
4. `clower.Lower(ast)` — MEP-45 aotir lowerer, produces
   `*aotir.Program`.
5. `colour.Compute(prog)` — PHP colour pass (all-Blue).
6. `lower.Lower(prog, colours)` — PHP-specific lowerer, produces
   `*ptree.PhpFile`.
7. `os.MkdirAll(outDir, 0o755)` — creates the output directory.
8. `emit.Emit(file, outDir, "main")` — writes `outDir/main.php`.
9. If `TargetPhpRun`: `resolvePhp()` then `exec.Command(php, main.php)`.

The function also touches unused fields to keep the compiler honest:
```go
_ = srcBytes
_ = d.cacheKey
_ = d.effectiveCacheDir
_ = copyFile
_ = sha256.New
_ = io.Copy
```
This pattern prevents dead-code removal of symbols that will be wired in
future phases without requiring a phase-gated build tag.

## 4. resolvePhp

`resolvePhp()` (build.go lines 128-145) finds the PHP binary:

1. Check `PHP_PATH` environment variable. If it points to a directory,
   append `/php`.
2. Try well-known paths: `/usr/bin/php`, `/usr/local/bin/php`,
   `/opt/homebrew/bin/php`.
3. Fall back to `exec.LookPath("php")`.

Error message on failure: `"php not found on PATH (set PHP_PATH or add
php to PATH)"`. CI sets `PHP_PATH` via the `shivammathur/setup-php@v2`
action output.

## 5. effectiveCacheDir

`effectiveCacheDir()` (build.go lines 148-160) resolves the build cache
directory:

1. `d.CacheDir` if set.
2. `$MOCHI_CACHE_DIR/php` if `MOCHI_CACHE_DIR` is set.
3. `~/.cache/mochi/php/` as the default.
4. `os.TempDir()` if home directory resolution fails.

Currently unused in the build path (every build runs the full pipeline
from scratch). Reserved for a future cache integration.

## 6. cacheKey

`cacheKey(srcBytes)` (build.go lines 168-178) computes a SHA-256 hash
of: source bytes + phpPath + Deterministic flag byte. Currently unused
in `Build` (the `_ = d.cacheKey` line keeps it live). The design intent
is to use this key to skip re-lowering when the source and PHP version
have not changed.

## 7. runtimeSourceDir and copyFile

`runtimeSourceDir()` (build.go lines 183-195) uses `runtime.Caller(0)`
to locate the `transpiler3/php/runtime/` directory relative to the Go
source file. This works regardless of the working directory, which is
important for test isolation (each test uses `t.TempDir()`).

`copyFile(dst, src)` (build.go lines 199-215) copies a file, creating
parent directories as needed. Phase 15 uses this when staging the
Composer package: it copies `composer.json`, `src/`, and related files
from `runtimeSourceDir()` into the sandbox.

## 8. repoRootForBuild

`repoRootForBuild` (build.go lines 220-240) walks up from the Go source
file to find the `go.mod` root. Test helpers use `repoRoot(t)` (defined
in `build_test.go` lines 52-56) to find fixture directories
independently of the working directory. This pattern appears in all
`phase*_test.go` files.

## 9. Phase 17: Packaging

`transpiler3/php/build/packaging.go` implements three deployment targets.

### 9.1 Phar archive: emitPharStager

`emitPharStager(outDir, mainPhp, dstPhar)` (packaging.go lines 53-74)
generates a stager PHP script (`build_phar.php`) that wraps `mainPhp`
into a `.phar` using PHP's built-in `Phar` class:

```php
$phar = new Phar($dst, 0, basename($dst));
$phar->startBuffering();
$phar->addFile($src, 'main.php');
$phar->setStub($phar->createDefaultStub('main.php'));
$phar->stopBuffering();
```

The stager is run with `php -d phar.readonly=0` (phase17_test.go line 74)
to bypass the default `phar.readonly = 1` INI setting. The resulting
`.phar` runs with `php out.phar` without any special flags.

`phpStringLit(s)` (packaging.go lines 200-215) escapes the file paths
as single-quoted PHP literals, avoiding double-quote interpolation issues.

### 9.2 FrankenPHP bundle: EmitFrankenPHPBundle

`EmitFrankenPHPBundle(outDir, packageName)` (packaging.go lines 141-168)
writes two files:

**Caddyfile** (template `caddyfileTmpl`, lines 76-91):
```
{
    frankenphp {
        worker /app/main.php 4
    }
}

:8080 {
    root * /app
    php_server
}
```
- `worker /app/main.php 4`: starts 4 worker processes.
- `php_server`: modern FrankenPHP directive (not `php_fastcgi`).

**Dockerfile** (template `dockerfileTmpl`, lines 93-105):
```
FROM dunglas/frankenphp:php8.4
WORKDIR /app
COPY main.php /app/main.php
COPY Caddyfile /etc/caddy/Caddyfile
EXPOSE 8080
```
Pinned to `dunglas/frankenphp:php8.4`.

### 9.3 RoadRunner bundle: EmitRoadRunnerBundle

`EmitRoadRunnerBundle(outDir, packageName)` (packaging.go lines 171-195)
writes two files:

**.rr.yaml** (template `rrYamlTmpl`, lines 107-123):
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

**worker.php** (template `rrWorkerTmpl`, lines 125-138):
```php
<?php
declare(strict_types=1);
require_once __DIR__ . '/main.php';
// Real apps wire PSR-7 here...
```

### 9.4 TestPhase17AllTargetsTogether

`TestPhase17AllTargetsTogether` (phase17_test.go lines 206-246) runs
all three targets for every Phase 17 fixture in one test, asserting
that all five artifacts (`build_phar.php`, `Caddyfile`, `Dockerfile`,
`.rr.yaml`, `worker.php`) are produced. This cross-cut gate ensures a
regression in any one packaging path fails the whole suite.

## 10. CI workflow

`.github/workflows/transpiler3-php-test.yml` has two jobs:

### 10.1 go-side

- Runs on `ubuntu-latest`.
- Installs PHP 8.4 via `shivammathur/setup-php@v2` with
  `extensions: mbstring, gmp` and `tools: composer:v2`.
- Runs `go vet ./transpiler3/php/...`, `go build`, `go test`.
- This job covers the full Go test suite including all fragment tests,
  the DJB2 hash tests, the reproducibility tests, and the packaging
  structure tests.

### 10.2 php-runtime

- Runs on `ubuntu-latest` with a PHP version matrix.
- Runs `composer install --no-interaction --prefer-dist` in
  `transpiler3/php/runtime/`.
- Runs PHPStan, Psalm, php-cs-fixer (dry-run), and PHPUnit.
- PHP 8.4.0 and 8.4 latest: `allow_failure: false`.
- PHP 8.5: `allow_failure: true`.

### 10.3 Timeout

Both jobs have `timeout-minutes: 15`. The go-side job is fast (no PHP
compilation of fixture programs needed for fragment tests). The
php-runtime job spends most of its time in Composer install and Psalm
analysis.
