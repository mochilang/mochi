---
title: "MEP-56 research bundle: Mochi to Ruby transpiler"
description: "Twelve research notes covering language surface, design philosophy, prior art, runtime, codegen, type lowering, Ruby target portability, dataset pipeline, agents and streams, build system, testing gates, and risks for the Mochi-to-Ruby transpiler proposed in MEP-56."
sidebar_position: 56
sidebar_label: "MEP-56"
---

# MEP-56 research bundle: Mochi to Ruby transpiler

Author: research pass for MEP-56 (Mochi to Ruby transpiler).
Date: 2026-05-29 (GMT+7).

This bundle contains the twelve research notes that informed [MEP-56, the Mochi-to-Ruby transpiler](/docs/mep/mep-0056). The notes are informative; the normative spec is in the MEP body.

| # | Title | Topic |
|---|-------|-------|
| 01 | [Language surface](/docs/research/0056/language-surface) | Mochi features mapped onto CRuby 3.2+ / JRuby / TruffleRuby lowering obligations |
| 02 | [Design philosophy](/docs/research/0056/design-philosophy) | Why Ruby 3.2 floor, why Data.define records, why Thread::SizedQueue streams |
| 03 | [Prior-art transpilers](/docs/research/0056/prior-art-transpilers) | Opal (Ruby-to-JS), RubyMotion, Crystal, mruby, JRuby, TruffleRuby, Artichoke |
| 04 | [Runtime building blocks](/docs/research/0056/runtime) | CRuby stdlib, Thread, Fiber, Data.define, Enumerator::Lazy, net/http, json |
| 05 | [Codegen design](/docs/research/0056/codegen-design) | Ruby source via rtree IR, aotir reuse, 2-space indent, rubocop-compatible output |
| 06 | [Type-system lowering](/docs/research/0056/type-lowering) | Mochi types onto Integer/Float/String/Array/Hash/Set/Data.define/lambda |
| 07 | [Ruby target and portability](/docs/research/0056/ruby-target-portability) | CRuby 3.2/3.4/4.0, JRuby 10, TruffleRuby 33, mruby 4 subset, Tebako binary |
| 08 | [Dataset pipeline](/docs/research/0056/dataset-pipeline) | Query DSL via Enumerable::Lazy, group_by with sorted keys, compile-time Datalog |
| 09 | [Agents and streams](/docs/research/0056/agent-streams) | Agents as Ruby classes + Thread, Thread::SizedQueue MPMC broadcast streams |
| 10 | [Build system](/docs/research/0056/build-system) | RubyGems gemspec, Bundler, IRuby kernel, Tebako packaging, TruffleRuby native |
| 11 | [Testing gates](/docs/research/0056/testing-gates) | Per-phase Go test gates, vm3 oracle, CRuby/JRuby/TruffleRuby matrix in CI |
| 12 | [Risks and alternatives](/docs/research/0056/risks-and-alternatives) | mruby subset limitations, TruffleRuby polyglot API, Tebako signing, cassette drift |

The companion MEP body lives at [/docs/mep/mep-0056](/docs/mep/mep-0056). Implementation tracking lives at [/docs/implementation/0056/](/docs/implementation/0056/).
