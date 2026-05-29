// Package build is the JVM-style end-to-end driver for the Mochi Ruby
// transpiler. Driver.Build resolves a Ruby toolchain, parses+type-checks the
// Mochi source, lowers via the shared aotir to an rtree.SourceFile, emits .rb,
// and (depending on Target) packages it as a runnable script, a gem, a bundle,
// an IRuby kernel, a Tebako bin, a TruffleRuby native image, or an mruby
// binary. Phase 0 ships only TargetRubySource (writes .rb files to a directory).
package build
