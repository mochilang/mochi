---
title: "04. RBS and YARD ingest"
sidebar_position: 5
sidebar_label: "04. RBS and YARD ingest"
description: "The RBS type signature format, how the bridge extracts API surfaces from bundled .rbs files and gem_rbs_collection, the YARD doc fallback, and the Go-side parser shape for walking RBS method declarations."
---

# 04. RBS and YARD ingest

This note documents how MEP-76's ingest pipeline turns a Ruby gem into a machine-readable surface description. The pipeline runs at `mochi pkg lock` time, not at build time or at runtime.

## RBS format overview

RBS (Ruby Signature) is the official Ruby 3.0+ type system, introduced in December 2020 alongside Ruby 3.0 and maintained in the `ruby/rbs` repository. RBS files use the `.rbs` extension and live under the `sig/` directory of a gem.

A representative `.rbs` file:

```rbs
module Nokogiri
  class HTML5
    def self.parse: (String input, ?String url, ?String encoding, ?Integer max_errors, ?Hash[Symbol, untyped] options) -> Document

    class Document < XML::Document
      def url: () -> String?
      def css: (String selector) -> XML::NodeSet[XML::Node]
      def at_css: (String selector) -> XML::Node?
      def title: () -> String
      def title=: (String value) -> String
    end
  end
end
```

The key RBS declarations the bridge parses:

- **`MethodDefinition`**: a method signature. Carries the method name, parameter types, return type, and the visibility (public/private). A single method may have multiple overload signatures separated by `|`.
- **`Attribute`**: an `attr_reader`, `attr_writer`, or `attr_accessor` declaration. Synthesised into getter/setter methods by the bridge.
- **`ClassDecl`**: a class definition. Carries the class name, superclass, included modules, and a list of member declarations.
- **`ModuleDecl`**: a module definition. Carries the module name and members.
- **`InterfaceDecl`**: an interface (prefixed with `_`, e.g., `_ToS`). Used for structural typing; the bridge maps these to Mochi interface types where possible.
- **`TypeAlias`**: a `type Name = Type` declaration. The bridge expands aliases inline.

RBS type vocabulary relevant to the closed type table:

| RBS type | Notes |
|----------|-------|
| `Integer` | Arbitrary-precision in Ruby; maps to Mochi `int` (64-bit). |
| `Float` | IEEE 754 double; maps to Mochi `float`. |
| `String` | UTF-8 string; maps to Mochi `string`. |
| `Symbol` | Interned symbol; maps to Mochi `string` (coerced). |
| `bool` | `true` or `false`; maps to Mochi `bool`. |
| `nil` | Null; maps to Mochi `nil`. |
| `Array[T]` | Typed array; maps to Mochi `[T]`. |
| `Hash[K, V]` | Maps to Mochi `map[K]V`. |
| `untyped` | Dynamic; maps to Mochi `any`. |
| `top` | Supertype of all types; maps to `any`. |
| `bot` | Subtype of all types; maps to `never`. |
| `void` | Return-type-only; maps to Mochi `void`. |
| `self` | The receiver type; contextual, resolved by the bridge. |
| `instance` | An instance of the enclosing class; maps to the synthesised extern type. |
| `T?` | Shorthand for `T \| nil`; maps to Mochi `T?`. |
| `T \| U` | Union; maps to Mochi `T \| U` where both sides are in the closed table. |
| `^(A) -> B` | Proc/lambda type; deferred to phase 4b. |

Types outside this table (e.g., `Proc`, `Method`, `Binding`, `IO`, complex multi-union types involving abstract classes) produce `SkipReport` entries.

## Locating `.rbs` files in a gem tarball

A gem tarball (`.gem` file) is a gzip-compressed tar archive. The bridge unpacks it to a content-addressed temp directory and walks two conventional locations:

1. `sig/**/*.rbs` (the convention adopted by most modern gems).
2. `spec/sig/**/*.rbs` (an older convention used by some gems that moved the `sig/` directory under `spec/` to keep the gem root clean).

Gems also declare metadata in the gemspec:

```ruby
spec.metadata["source_code_uri"] = "https://github.com/sparklemotion/nokogiri"
spec.metadata["changelog_uri"]   = "https://github.com/sparklemotion/nokogiri/CHANGELOG.md"
```

These URIs are informational for the bridge (used to construct the `gem_rbs_collection` lookup key and to populate `mochi.lock` provenance fields), not for locating `.rbs` files. The bridge always looks inside the tarball first; fetching external sources only happens via `gem_rbs_collection` (see below).

## gem_rbs_collection

The `ruby/gem_rbs_collection` repository (github.com/ruby/gem_rbs_collection) is a community-maintained collection of RBS signatures for popular gems that have not yet migrated their `.rbs` files into the gem itself. As of May 2026, the collection covers 400+ gems.

The repository ships a manifest file at `gems.json`:

```json
{
  "gems": [
    {
      "name": "redis",
      "versions": {
        "5.0": { "path": "gems/redis/5.0", "rbs_gem_version": "0.1.0" }
      }
    },
    {
      "name": "sinatra",
      "versions": {
        "3.2": { "path": "gems/sinatra/3.2", "rbs_gem_version": "0.2.0" }
      }
    }
  ]
}
```

At `mochi pkg lock` time, the bridge:

1. Downloads `gems.json` from the pinned commit SHA stored in `mochi.lock` (first lock: fetches HEAD of the default branch and pins it).
2. For each gem in `[ruby-dependencies]` that has no bundled `.rbs` files, looks up the gem's resolved version in `gems.json`.
3. Downloads the matching `.rbs` subtree (the `path` field is relative to the repo root).
4. Computes SHA256 of each downloaded `.rbs` file.
5. Writes `[[ruby-package]]` entries into `mochi.lock` with `rbs-source = "gem_rbs_collection"` and `rbs-collection-sha256 = "<hex>"`.

On subsequent `mochi pkg lock` runs, the SHA256 is verified against the stored value. Any mismatch aborts with a diagnostic.

## YARD fallback

If a gem has neither bundled `.rbs` files nor a `gem_rbs_collection` entry, the bridge falls back to YARD documentation. YARD parses `@param [Type] name` and `@return [Type]` comment tags:

```ruby
# Parse an HTML document.
#
# @param input [String] the raw HTML string
# @param url   [String, nil] optional base URL
# @return [Nokogiri::HTML5::Document] the parsed document
def parse(input, url = nil)
  ...
end
```

YARD type expressions are freeform strings. The bridge applies a best-effort parser that recognises these forms:

| YARD expression | Mochi type |
|-----------------|------------|
| `String` | `string` |
| `Integer` | `int` |
| `Float` | `float` |
| `Boolean` | `bool` |
| `nil` | `nil` |
| `Symbol` | `string` |
| `Array<String>` | `[string]` |
| `Array<Integer>` | `[int]` |
| `Hash{Symbol => Integer}` | `map[string]int` |
| `String, nil` | `string?` |
| `Integer, nil` | `int?` |

Anything the YARD parser does not recognise (complex multi-type unions, custom class names not in the closed table, `#each` duck types) becomes `untyped` and the item is added to the `SkipReport` with `reason = "YARD-unresolved"`.

YARD ingest accuracy from the top-50 RubyGems survey (May 2026):

| Coverage source | Gem count | Notes |
|-----------------|-----------|-------|
| Bundled `.rbs` | 21 | High accuracy, machine-validated. |
| `gem_rbs_collection` | 17 | High accuracy, community-maintained. |
| YARD only | 9 | Partial accuracy; YARD-unresolved items skipped. |
| None | 3 | Runtime-introspection path, deferred to phase 4b. |

## Go-side parser shape

The bridge reads `.rbs` text and walks a minimal grammar subset. The bridge does NOT invoke the `rbs` gem CLI (that would require Ruby at lock time, introducing a circular bootstrap dependency: to lock Ruby gems, we would need Ruby to parse their signatures). Instead, MEP-76 implements a lightweight RBS parser in Go covering the declarations needed to populate the shim.

The parser lives at `package3/ruby/rbs/parser.go` (approximately 400 LOC). It covers:

- `ClassDecl` and `ModuleDecl` (nested or top-level).
- `MethodDefinition` with single and multi-overload signatures.
- `Attribute` declarations (`attr_reader`, `attr_writer`, `attr_accessor`).
- `TypeAlias` expansion.
- All type forms in the closed table (see §RBS format overview above).

Items the Go parser does not understand (interface declarations with complex structural constraints, `extend self` module patterns, RBS 3.x `overload` annotations) produce `SkipReport` entries and are not surfaced in the shim.

The parser output is a normalised `GemSurface` value:

```go
package rbs

type GemSurface struct {
    GemName    string
    GemVersion string
    RBSVersion string
    Classes    []ClassDef
    Modules    []ModuleDef
    Skipped    []SkipReport
}

type ClassDef struct {
    Name       string
    SuperClass string
    Methods    []MethodSig
    Attrs      []AttrSig
}

type MethodSig struct {
    Name       string
    Receiver   ReceiverKind  // Instance | Singleton
    Overloads  []Overload
}

type Overload struct {
    Params  []ParamSig
    Return  MochiType
}

type SkipReport struct {
    Item   string
    Reason string
}
```

The shim synthesiser walks `GemSurface` and emits the `shim.mochi` + `shim.rb` pair described in [[01-language-surface]] §Shim file mechanics.

## RBS grammar stability

RBS 3.x (shipped with Ruby 3.3+) is the target grammar version for MEP-76. The RBS grammar is version-pinned by the `ruby/rbs` repository's CHANGELOG and by the `rbs_version` field in each gem's `mochi.lock` entry.

The Go parser targets the RBS 3.x grammar. Breaking changes to the RBS grammar between versions are tracked in `ruby/rbs` CHANGELOG. When the bridge encounters an `.rbs` file that declares a `rbs_version` higher than the parser supports, it emits a warning and parses on a best-effort basis.

The bridge records per-gem RBS provenance in `mochi.lock`:

```toml
[[ruby-package]]
name = "nokogiri"
version = "1.16.4"
source = "rubygems"
gem-sha256 = "abc123..."
rbs-source = "bundled"
rbs-version = "3.3"
rbs-sha256 = "def456..."
```

For `gem_rbs_collection` gems, `rbs-source = "gem_rbs_collection"` and `rbs-collection-commit = "<sha>"` is added.

## Cross-references

- [[01-language-surface]] for the user-visible surface this ingest feeds (shim file mechanics, import resolution).
- [[02-design-philosophy]] §3-4 for the RBS vs YARD decision and `gem_rbs_collection` rationale.
- [[03-prior-art]] §Ruby C extension API for why C extension gem methods often lack bundled RBS.
- [[05-type-mapping]] for the closed table that maps RBS types to Mochi types.
- [[12-risks-and-alternatives]] for the deferred phase 4b runtime-introspection path and the 3 gems with no coverage.
- [MEP-76](/docs/mep/mep-0076) for the normative pipeline spec.
