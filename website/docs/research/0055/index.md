---
title: "MEP-55 research bundle: Mochi to PHP 8.4 transpiler"
description: "Thirteen research notes covering language surface, design philosophy, prior art, runtime, codegen, type lowering, PHP target portability, dataset pipeline, agents and streams, build system, testing gates, and risks for the Mochi-to-PHP 8.4 transpiler proposed in MEP-55."
sidebar_position: 55
sidebar_label: "MEP-55"
---

# MEP-55 research bundle: Mochi to PHP 8.4 transpiler

Author: research pass for MEP-55 (Mochi-to-PHP 8.4 transpiler).
Date: 2026-05-29 15:00 (GMT+7).

This bundle contains the thirteen research notes that informed [MEP-55, the Mochi-to-PHP 8.4 transpiler](/docs/mep/mep-0055). The notes are informative; the normative spec is in the MEP body.

| # | Title | Topic |
|---|-------|-------|
| 01 | [Language surface](/docs/research/0055/01-language-surface) | Mochi language surface mapped onto PHP 8.4 lowering obligations: scalars, collections, records, sum types, closures, match, loops, query DSL, phase corpus |
| 02 | [Design philosophy](/docs/research/0055/02-design-philosophy) | Why PHP, why PHP 8.4 floor, why `declare(strict_types=1)`, why sync wrappers not Amp/Revolt, why `final readonly class` for records, why `abstract readonly class` bases, why DJB2/GMP, why Phar over box |
| 03 | [Prior art: transpilers](/docs/research/0055/03-prior-art-transpilers) | Survey of source-to-PHP tools (Hack/HHVM, Haxe PHP target, Dart2PHP); PHP version history; why 8.4 is the minimum; why a direct aotir→ptree→emit approach |
| 04 | [Runtime](/docs/research/0055/04-runtime) | The `mochi/runtime` Composer package: directory structure, IO class, `@api` annotation, devDependencies (Psalm 6, PHPStan 1.12, php-cs-fixer), zero runtime deps, required extensions |
| 05 | [Codegen design](/docs/research/0055/05-codegen-design) | aotir → ptree → emit pipeline: ptree node types, `runtimeFlags` struct, `mochi__` prefix, emit pass, pipeline wiring in Driver.Build |
| 06 | [Type lowering](/docs/research/0055/06-type-lowering) | Per-Mochi-type lowering rules for every type: int, float, bool, string, list, map, set, record, sum type, closures, function types, Result, panic |
| 07 | [PHP target portability](/docs/research/0055/07-php-target-portability) | PHP ecosystem portability: CI matrix (8.4.0 / 8.4 / 8.5 allow_failure), phar.readonly=0, FrankenPHP vs RoadRunner, Composer 2, Packagist, server-model implications |
| 08 | [Dataset pipeline](/docs/research/0055/08-dataset-pipeline) | Query DSL lowering (from/where/select/order-by/skip/take onto PHP arrays + usort), Phase 8 Datalog compile-time semi-naive evaluation, static array literal output |
| 09 | [Agents and streams](/docs/research/0055/09-agent-streams) | Phase 9 agents (mutable PHP class), Phase 10 streams (MochiStream/MochiSub fan-out, backpressure), Phase 11 async coloring (all-Blue, sync wrappers) |
| 10 | [Build system](/docs/research/0055/10-build-system) | Driver.Build pipeline, resolvePhp, effectiveCacheDir + cacheKey (reserved), Phase 17 packaging (emitPharStager, EmitFrankenPHPBundle, EmitRoadRunnerBundle), CI workflow |
| 11 | [Testing gates](/docs/research/0055/11-testing-gates) | Per-phase test structure, runPhpFixture, TestPhaseNEmitFragments, TestPhase13DJB2HashMatchesCassetteFilenames, runPhpLLMFixture, runPharFixture, TestPhase17AllTargetsTogether, two-tier strategy |
| 12 | [Risks and alternatives](/docs/research/0055/12-risks-and-alternatives) | Risk register: PHP scheduling, PHP_INT_MAX DJB2 overflow, Psalm 5 vs 6, abstract readonly inheritance, phar.readonly, amphp/revolt removal, live LLM deferral, PHP 8.5 allow_failure |

## Cross-references

- [MEP-45 (C target)](/docs/mep/mep-0045)
- [MEP-46 (BEAM target)](/docs/mep/mep-0046)
- [MEP-47 (JVM target, direct bytecode)](/docs/mep/mep-0047)
- [MEP-48 (.NET target)](/docs/mep/mep-0048)
- [MEP-49 (Swift target)](/docs/mep/mep-0049)
- [MEP-50 (Kotlin target)](/docs/mep/mep-0050)
- [MEP-51 (Python target)](/docs/mep/mep-0051)
- [MEP-52 (TypeScript/JavaScript target)](/docs/mep/mep-0052)
- [MEP-53 (Rust target)](/docs/mep/mep-0053)
- [MEP-54 (Erlang/OTP target)](/docs/mep/mep-0054)
