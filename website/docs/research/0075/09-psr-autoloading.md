---
title: "09. PSR-4 autoloading"
sidebar_position: 10
sidebar_label: "09. PSR-4 autoloading"
description: "PSR-4 autoloading deep dive: why PSR-4 is the only viable layout for published Composer libraries, how class FQCN maps to file paths, how Composer generates autoload maps, how MEP-75 emits PSR-4-compliant src/ trees for TargetPhpLibrary, why PSR-0 is deprecated, and vendor-prefix collision avoidance."
---

# 09. PSR-4 autoloading

Author: research pass for MEP-75 (Mochi and PHP package bridge). Date: 2026-05-29 22:11 (GMT+7).

This note explains PHP's autoloading system, why PSR-4 is the standard for modern Composer packages, and how `TargetPhpLibrary` emits a PSR-4-compliant tree.

## 1. PHP's autoloading mechanism

PHP's autoloading is triggered by the `spl_autoload_register()` function. When PHP encounters an undefined class name, it iterates registered autoloaders. Each autoloader receives the fully qualified class name (FQCN) and is expected to `require_once` the file that defines it.

Composer generates a single `vendor/autoload.php` file that registers a class-map-based autoloader plus PSR-4/PSR-0 autoloaders for each installed package. Once a program `require_once 'vendor/autoload.php'`, every installed class is available on demand without manual `require` calls.

## 2. PSR-0 (deprecated)

PSR-0 (PHP-FIG, accepted 2009, deprecated 2014) maps the FQCN to a file path by converting namespace separators (`\`) and underscores to directory separators:

- Class `GuzzleHttp_Client` maps to `GuzzleHttp/Client.php`.
- Class `GuzzleHttp\Client` maps to `GuzzleHttp/Client.php`.
- Class `Symfony\Component\Console\Command` maps to `Symfony/Component/Console/Command.php`.

The root directory for the mapping is configurable. The main limitation: underscores in class names are treated as namespace separators, causing ambiguity for classes that use underscores in their non-namespace names.

PSR-0 is deprecated. Packagist still accepts PSR-0 packages but does not recommend them. Composer 2.x supports PSR-0 for backward compatibility but warns on new packages using it. MEP-75 does not emit PSR-0 autoload mappings.

## 3. PSR-4

PSR-4 (PHP-FIG, accepted 2013) maps a namespace prefix to a base directory. The class FQCN is resolved by removing the registered prefix and converting the remaining namespace parts to directory separators:

- Prefix `GuzzleHttp\` maps to `src/`.
- Class `GuzzleHttp\Client` resolves to `src/Client.php`.
- Class `GuzzleHttp\Handler\CurlHandler` resolves to `src/Handler/CurlHandler.php`.

The PSR-4 mapping in `composer.json`:

```json
{
    "autoload": {
        "psr-4": {
            "GuzzleHttp\\": "src/"
        }
    }
}
```

Multiple prefixes are allowed:

```json
{
    "autoload": {
        "psr-4": {
            "App\\": "app/",
            "App\\Tests\\": "tests/"
        }
    }
}
```

PSR-4 is strictly a prefix-to-directory mapping with no path manipulation for underscores. It is unambiguous and efficient (Composer generates a direct lookup map).

## 4. How Composer generates the autoload map

When `composer install` or `composer dump-autoload` is run, Composer:

1. Reads the `autoload.psr-4` mappings from each installed package's `composer.json`.
2. Writes `vendor/autoload.php` (the entry point that `require_once`s the loader).
3. Writes `vendor/composer/autoload_psr4.php` (the PSR-4 prefix-to-directory map).
4. Writes `vendor/composer/autoload_classmap.php` (an optional class-map for packages that use class-map autoloading or have a large number of classes).
5. Writes `vendor/composer/autoload_real.php` and `vendor/composer/autoload_static.php` (the actual autoloader code).

For packages with explicit `autoload.classmap` entries, Composer also walks the source directories and builds a file-level class map (every `.php` file is parsed for class declarations; the map stores `ClassName => /path/to/file.php`).

## 5. How MEP-75 pre-computes the autoload map

For the consume direction, MEP-75 does NOT run `composer install` at build time (it would require network access and a Composer installation). Instead, the bridge pre-computes the autoload map from the lockfile data:

1. For each `[[php-package]]` in `mochi.lock`, read the `autoload.psr-4` mappings from the package's `composer.json` (extracted from the content-addressed cache).
2. Build a prefix-to-directory map in memory:
   ```
   "GuzzleHttp\\" => "<vendor-dir>/guzzlehttp/guzzle/src/"
   "React\EventLoop\\" => "<vendor-dir>/react/event-loop/src/"
   ...
   ```
3. Emit a `vendor/autoload.php` that registers a single PSR-4 autoloader with the combined map (no `autoload_classmap.php` in the bridge's output; class-map packages get a simplified PSR-4 fallback with a SkipReport warning).
4. Emit the vendor directory structure (`vendor/<vendor>/<package>/`) by materialising the extracted packages from the content-addressed cache.

This pre-computed autoload is sufficient for MEP-55's `php main.php` execution model. The only packages that fail this approach are those that use `autoload.files` (unconditional file inclusion) or `autoload.classmap` without a PSR-4 fallback. These edge cases produce a SkipReport at lock time.

## 6. How TargetPhpLibrary emits PSR-4 layout

The `TargetPhpLibrary` emitter writes the following structure:

```
src/
  <PascalVendor>/
    <PascalPackage>/
      <ClassName>.php      (one per exported Mochi type)
      functions.php        (exported free functions)
```

The `composer.json` autoload section:

```json
{
    "autoload": {
        "psr-4": {
            "<psr4-namespace>": "src/"
        }
    }
}
```

Where `<psr4-namespace>` is derived from `[php.publish] psr4-namespace` in `mochi.toml`, defaulting to a PascalCase transformation of the Packagist package name:

- `my-vendor/my-package` → `MyVendor\\MyPackage\\`
- `acme/http-client` → `Acme\\HttpClient\\`

The class file naming follows PSR-4:

- Class `MyVendor\MyPackage\Client` lives in `src/MyVendor/MyPackage/Client.php`.
- The emitter writes exactly one class per file (standard PHP best practice).

## 7. Vendor-prefix collision avoidance

When multiple Packagist packages are imported, their PSR-4 prefixes must not collide. The bridge enforces uniqueness at lock time:

- Each imported package's PSR-4 prefix is recorded in the lockfile.
- If two packages declare the same root prefix (e.g., both declare `App\\`), the bridge emits a SkipReport and falls back to class-map autoloading for both.
- The MEP-75 glue stubs use a dedicated `MochiGlue\<PascalVendor>\<PascalPackage>\` namespace prefix, which is reserved and cannot conflict with upstream packages (no upstream package on Packagist uses the `MochiGlue\` prefix; the bridge emits a lock-time error if a conflict is detected).

## 8. PSR-4 and MEP-55's emit conventions

MEP-55's existing PHP emit pass generates PHP code without PSR-4 structure (it emits a single `main.php` or a Phar). `TargetPhpLibrary` adds PSR-4 structure on top of MEP-55's existing class-emit logic.

The key changes:

- Class declarations are split into one-class-per-file (MEP-55 puts all classes in `main.php`).
- File paths are derived from the class FQCN and the configured PSR-4 prefix.
- A `namespace <root-namespace>;` declaration is added to every file.
- The `declare(strict_types=1);` declaration (already added by MEP-55) is preserved.

## Cross-references

- [[06-composer-publish-flow]] for how `composer.json` is synthesised for the producer direction.
- [[10-build-system]] for how the vendor autoload map is pre-computed.
- [MEP-55 Phase 15](/docs/mep/mep-0055) for the Composer/PHPStan/Psalm integration.
- [MEP-75 §9](/docs/mep/mep-0075#9-targetphplibrary) for the normative TargetPhpLibrary spec.
