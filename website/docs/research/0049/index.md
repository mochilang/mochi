---
title: MEP-49 research notes
sidebar_position: 1
sidebar_label: Overview
description: "Research notes feeding MEP-49 (Mochi-to-Swift transpiler for iOS, macOS, Linux, Windows). Twelve notes covering language surface, Swift design philosophy, prior art on Swift transpilers and Apple toolchains, Swift runtime building blocks, codegen via SwiftSyntax, type lowering, Swift portability matrix, dataset pipeline, agents on Swift actors + AsyncStream, SwiftPM build system, testing, and risks."
---

# MEP-49 research notes

These twelve notes are the deep research that fed MEP 49 (Mochi-to-Swift transpiler). They are informative; the MEP body, once landed, will be normative. Each note is self-contained and can be read independently. Cross-references use `[[note-slug]]` markers.

| # | Title | Topic |
|---|---|---|
| 01 | [Language surface](/docs/research/0049/language-surface) | Mochi features mapped onto Swift 6.0 lowering obligations |
| 02 | [Design philosophy](/docs/research/0049/design-philosophy) | Why Swift, why Swift 6.0 floor, why all platforms, why actors + AsyncStream |
| 03 | [Prior-art transpilers](/docs/research/0049/prior-art-transpilers) | Skip.tools, J2ObjC, Swiftify, Sourcery, Hylo, Mojo, Embedded Swift, SwiftWasm, IL2CPP analogues, Macros, Obj-C / C++ interop |
| 04 | [Runtime building blocks](/docs/research/0049/runtime) | Swift stdlib, Foundation, Apple-only frameworks, swift-collections / -algorithms / -async-algorithms, AsyncStream / Task / actor surface |
| 05 | [Codegen design](/docs/research/0049/codegen-design) | Swift source via SwiftSyntax IR, aotir reuse, swift-format integration, monomorphisation, closure ABI |
| 06 | [Type-system lowering](/docs/research/0049/type-lowering) | Mochi types onto Int64 / Double / String / Array / Dictionary / OrderedDictionary / @frozen struct / enum with indirect cases |
| 07 | [Swift target and portability](/docs/research/0049/swift-target-portability) | Swift 6.0 / 6.1 / 6.2 matrix, Apple deployment targets, Static Linux SDK, Windows MSVC, Embedded / SwiftWasm exclusions |
| 08 | [Dataset pipeline](/docs/research/0049/dataset-pipeline) | Query DSL via Sequence / AsyncSequence + swift-collections + swift-algorithms, hash / merge / nested-loop joins |
| 09 | [Agents and streams](/docs/research/0049/agent-streams) | Swift actor + AsyncStream mailboxes, async colouring, AsyncSequence for streams |
| 10 | [Build system](/docs/research/0049/build-system) | SwiftPM canonical driver, deterministic Package.swift, xcodebuild + codesign + notarytool, WiX MSI |
| 11 | [Testing gates](/docs/research/0049/testing-gates) | Per-phase Go test gates, Swift version matrix, swiftc clean, swift-format fixed-point, App Store validation |
| 12 | [Risks and alternatives](/docs/research/0049/risks-and-alternatives) | Risk register, Objective-C / Swift 5.10 / Apple-only / GCD / Macros / SwiftSyntax-required / Combine rejected and why |

Each note's filename uses the `NN-slug.md` convention; the leading `NN-` is stripped by Docusaurus for the URL path, so cross-links inside the notes use the unprefixed slug (e.g. `[[language-surface]]`).

The companion MEP body lives at [/docs/mep/mep-0049](/docs/mep/mep-0049).
