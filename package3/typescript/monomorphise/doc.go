// Package monomorphise parses the `[[ts.monomorphise]]` manifest entries and
// emits per-instantiation extern entries for TS generics that cannot be
// type-erased (conditional types, mapped types, higher-kinded compositions).
// Lands in MEP-72 Phase 15.
//
// See website/docs/implementation/0072/phase-15-monomorphise.md.
package monomorphise
