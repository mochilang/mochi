---
title: "03. Prior-art bridges"
sidebar_position: 4
sidebar_label: "03. Prior-art bridges"
description: "Survey of HHVM/Hack, PHP FFI extension (ext-ffi), PHP-CPP, Node.js edge-php, Python php-serialize, Go/PHP via exec, phpize extension authoring, PhpBrew, and prior Mochi-to-PHP interop attempts. Comparison with how other languages bridge PHP. Why MEP-75 chose reflection-first over annotation-first."
---

# 03. Prior-art bridges

Author: research pass for MEP-75 (Mochi and PHP package bridge). Date: 2026-05-29 22:11 (GMT+7).

This note surveys the landscape of PHP interop projects: what each attempted, what constraints they imposed on users, and what MEP-75 borrows or deliberately avoids.

## HHVM and Hack

Facebook's HHVM (HipHop Virtual Machine, 2013-present) is a JIT-compiling PHP runtime with Hack as its type-safe superset language. Hack introduced type annotations (`int`, `string`, `?T`, `Vector<T>`, `Map<K, V>`) to PHP and added a gradual type checker.

HHVM's relevance to MEP-75: Hack's type annotations are the closest PHP has come to a machine-readable surface format. Hack classes and functions carry full generic type parameters. However, HHVM/Hack is a separate runtime from standard PHP; as of 2024, HHVM's standard PHP compatibility drifted enough that the vast majority of Packagist packages do not support HHVM. The Hack type system is not observable via PHP's standard Reflection API.

MEP-75 does not attempt to read Hack type annotations; the bridge targets packages that run on standard PHP 8.4.

## PHP FFI extension (ext-ffi)

PHP 7.4 introduced `FFI` (ext-ffi), which allows PHP code to call C functions and access C data structures directly from PHP. The extension requires a loaded shared library (.so/.dll) and a C header declaration.

```php
$ffi = FFI::cdef("int add(int a, int b);", "libmylib.so");
echo $ffi->add(1, 2); // → 3
```

ext-ffi is PHP-calling-C, not C-calling-PHP. There is no inverse API that allows a C program (or a Go program, or a Mochi-compiled native binary) to call into PHP via ext-ffi. The MEP-75 bridge does not use ext-ffi; the bridge is PHP-to-PHP.

## PHP-CPP

PHP-CPP (Emile Fugulin, 2013-2018) is a C++ library that lets developers write PHP extensions in C++. A PHP-CPP extension is a `libmyext.so` that PHP loads via `extension = myext.so`; from PHP's perspective it is a native extension indistinguishable from ext-json or ext-mbstring.

PHP-CPP allows C++ code to expose PHP classes and functions. The reverse (PHP calling user C++ code) is exactly what ext-ffi provides. PHP-CPP does not allow non-PHP code to call PHP.

MEP-75 does not use PHP-CPP. The bridge's PHP glue stubs are plain PHP files, not compiled extensions. Writing an extension would require C++ knowledge, platform-specific compilation, and a PHP build environment on every user machine; this violates the zero-boilerplate promise.

## Node.js edge-php (and similar runtimes)

Several projects have attempted to run PHP inside a JavaScript runtime:

- **edge-php** (Vercel, 2022-2023): runs PHP programs at the edge using a PHP WASM build. The PHP code runs inside a Wasm sandbox invoked from JavaScript.
- **PHP for the browser** (various Wasm experiments): compile PHP itself to WebAssembly via Emscripten, then run PHP programs inside a browser tab or a Node.js Wasm runtime.

These projects bridge PHP-as-a-runtime-environment into JavaScript environments. They do not provide a way to call PHP library code from a typed language like Mochi. The execution model is "run a PHP process inside Wasm", not "call a PHP class method from a typed call site".

MEP-75 does not use Wasm PHP runtimes. The bridge calls PHP packages via the standard PHP CLI, keeping MEP-55's existing `php main.php` execution model.

## Python php-serialize and similar

Various Python packages (`php-serialize`, `phpserialize`, `phply`) can parse PHP's serialised data format or parse PHP source. These are data-format bridges, not code bridges. They allow Python to read PHP session data or PHP's `serialize()`/`unserialize()` format, not to call PHP functions.

No Python project provides a general mechanism for calling arbitrary PHP class methods from Python. The most common approach for Python-to-PHP integration in practice is HTTP: Python calls a PHP application over HTTP (a REST API). MEP-75 does not use this approach; the bridge is same-process (Mochi-compiled-to-PHP calling Composer packages via PHP's own autoloader).

## Go/PHP via exec

The standard approach for Go programs to invoke PHP code is `exec.Command("php", "script.php", ...)`. This is process-level bridging: Go spawns a PHP process, PHP runs to completion, Go reads stdout. It is slow (~50-100ms per call for PHP interpreter startup), process-scoped (no shared state), and brittle (output parsing requires careful escaping).

MEP-75 uses `exec.Command("php", "reflect.php", packagePath)` only for the reflection CLI at lock time (a one-time operation per package version). It does not use exec-based PHP invocation at runtime; the bridge targets the MEP-55 PHP output, which is already a PHP process.

## phpize and PHP extension authoring

`phpize` is the tool that prepares a PHP source directory for building a native extension. Extension authoring via the Zend API (`ZEND_FUNCTION`, `PHP_RINIT_FUNCTION`, etc.) gives full access to PHP's internals. However:

- Extension authoring requires C knowledge.
- The Zend API changes across PHP major versions (though it has been more stable in PHP 8.x).
- Extensions must be compiled per PHP version per platform.
- Installing a custom extension requires `php.ini` modification and root access on shared hosting.

MEP-75 does not write PHP extensions. The bridge's PHP glue stubs are pure PHP files that run via the standard PSR-4 autoloader.

## PhpBrew

PhpBrew is a tool for compiling and managing multiple PHP versions side-by-side (analogous to rbenv for Ruby or pyenv for Python). It is infrastructure tooling, not a code bridge.

MEP-75's CI matrix uses the standard `ubuntu-24.04` PHP packages (PHP 8.4.0 and 8.4 latest), not PhpBrew. The `[php] php-version` setting in `mochi.toml` declares the target version floor; the bridge validates the host PHP version against it.

## Prior Mochi-to-PHP interop attempts

Before MEP-55, Mochi had no PHP output target. There was therefore no official Mochi-to-PHP interop story. Informal experiments in the Mochi community included:

- Running Mochi programs through the vm3 interpreter and serialising their output to PHP-readable JSON. Viable for simple data exchange but not for calling PHP library code.
- Using MEP-45's C output to write a thin PHP extension that calls into Mochi-compiled native code via ext-ffi. Requires C knowledge, platform compilation, and a PHP build environment.

MEP-55 settled on the "Mochi source compiles to PHP source" model, which is the correct architecture for the PHP ecosystem. MEP-75 builds on that architecture.

## How other language bridges handle PHP

The pattern across all successful PHP interop stories is: PHP calls the foreign language, not the other way around. Examples:

- **Python calling PHP**: typically via `subprocess.run(["php", "script.php"])` or via HTTP to a PHP application server.
- **Go calling PHP**: via `exec.Command("php", ...)` or via HTTP.
- **Rust calling PHP**: via `std::process::Command` (exec model) or via the PHP-embed SAPI (`libphp`; unstable, rarely used, requires a specific build of PHP).
- **JavaScript calling PHP**: via Wasm (edge-php) or HTTP.

In every case, the integration point is either exec/HTTP (slow, process-scoped) or Wasm (PHP compiled to Wasm, which is a separate runtime). There is no established pattern for a compiled language to call PHP library code in the same process via a stable ABI.

MEP-75's design acknowledges this reality. The bridge operates in the same-process PHP model (MEP-55's output is already PHP; vendor packages are loaded via Composer's autoloader into the same PHP process) rather than attempting a cross-process or cross-ABI bridge.

## Why reflection-first over annotation-first

The annotation-first approach (used by MEP-73's rustdoc JSON: the crate author writes Rust, the tool reads the declared annotations) is ideal when:

1. The source language has a machine-readable annotation format (rustdoc JSON, go/types).
2. The annotations are maintained by the upstream author and ship with the package.
3. The annotations cover the full public surface.

PHP has no equivalent to rustdoc JSON. PHPDoc is close, but PHPDoc is embedded in source comments (not a separate machine-readable file), not all packages have complete PHPDoc, and PHPDoc is not normative (it is documentation, not a type contract).

Psalm stubs (`.phpstub` files) are more precise but are only shipped by a small fraction of Packagist packages.

The PHP Reflection API is the only universally available, runtime-accurate source of PHP type information. It is available in every PHP installation, stable across PHP 8.x, and reflects the actual runtime type enforcement (what PHP checks at call time). PHPDoc and Psalm stubs are used as augmentation, not as the primary source.

The trade-off: the Reflection API sees fewer types than a static analysis tool would (array shapes, generics, conditional returns are invisible to the runtime API). The bridge compensates with the SkipReport mechanism and the PHPDoc augmentation pass. The closed type table ensures that the visible information is translated correctly; the invisible information is refused rather than guessed at.

## Cross-references

- [[04-packagist-ingest]] for the Packagist v2 API and the reflection CLI design.
- [[05-type-mapping]] for the closed type table.
- [[02-design-philosophy]] §1 for the Reflection API vs PHP-Parser vs PHPStan comparison.
- [MEP-73 research/03](/docs/research/0073/03-prior-art-bridges) for the Rust bridge's prior-art survey (PyO3, neon, napi-rs, uniffi, diplomat).
- [MEP-74 research/03](/docs/research/0074/03-prior-art-bridges) for the Go bridge's prior-art survey.
