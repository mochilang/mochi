# MEP-75 Phase 11: Interface and Abstract Class Bridge (Hierarchy)

**Status**: LANDED 2026-05-30 00:33 (GMT+7)

## Goal

Implement hierarchy analysis for PHP class/interface relationships under `package3/php/externemit/hierarchy.go`. Given a `ReflectionSurface`, build an index of which classes extend which parents, which concrete classes implement each interface, and which classes are abstract. Use this index to emit `extern type` declarations with annotations listing concrete subtypes.

## Design

`BuildHierarchy(surface)` constructs a `Hierarchy` struct with five maps:

- `ClassByFQCN` - all classes indexed by FQCN
- `InterfaceByFQCN` - all interfaces indexed by FQCN
- `ImplementorsOf` - interface FQCN to list of implementing class FQCNs
- `SubclassesOf` - parent FQCN to list of direct child FQCNs
- `AbstractClasses` - set of abstract class FQCNs

`ConcreteImplementors(ifaceFQCN)` filters `ImplementorsOf` to non-abstract classes.

`AllSubclasses(fqcn)` performs a DFS walk over `SubclassesOf` to collect all transitive subclasses (cycle-safe via seen map).

`EmitAbstractBridge(surface)` emits `extern type` declarations for:
- Abstract classes, with a comment listing concrete (non-abstract) subclass handles
- Interfaces, with a comment listing concrete implementor handles

Concrete classes are skipped since they are handled by the main `Emit()` path.

## Files Landed

- `package3/php/externemit/hierarchy.go` -- Hierarchy + BuildHierarchy + EmitAbstractBridge
- `package3/php/externemit/hierarchy_test.go` -- 14 test functions

## Test Coverage

- ClassByFQCN and InterfaceByFQCN indexing
- AbstractClasses set membership
- SubclassesOf direct child mapping
- ImplementorsOf interface-to-class mapping
- AllSubclasses transitive DFS walk
- AllSubclasses no-cycle guard on leaf nodes
- ConcreteImplementors filters abstract classes
- InterfacesOf known and unknown class
- EmitAbstractBridge emits extern type for abstract classes
- EmitAbstractBridge emits concrete subclasses annotation
- EmitAbstractBridge emits Interface implementors annotation
- EmitAbstractBridge returns empty output for empty surface
- EmitAbstractBridge skips concrete-only surfaces
