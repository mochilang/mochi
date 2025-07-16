# Smalltalk Compiler Tasks

## Recent Enhancements
- Added fallback support for struct literals by emitting dictionaries. This reduces Rosetta `.error` files.
- Added golden tests for `tests/vm/valid` programs verifying generated
  Smalltalk code and runtime output.

## Remaining Work
- Support union type constructors.
