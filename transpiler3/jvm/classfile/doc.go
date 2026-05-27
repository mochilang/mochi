// Package classfile provides a ClassFile API (JEP 484) hot path for
// generating .class files directly without going through Java source.
// Used for sum-type dispatch shims, agent trampolines, and lambda
// bootstrap call sites. Stub in Phase 0; implemented in Phases 5-6.
package classfile
