# Dart Compiler Tasks

## Recent Enhancements (2025-07-13 05:15 UTC)
- Added ability to compile `tests/dataset/tpc-h/q1.mochi` through `q22.mochi` and generate runnable Dart code.
- README in `tests/machine/x/dart` now tracks TPCH progress with nicer checklist.
- JOB queries `q1`–`q10` now compile and run. Golden code files have `.dart` suffix instead of `.dart.out`.
- [2025-07-13 16:53 UTC] Updated TPCH compiler script to generate and execute queries `q1`–`q15` successfully. Fixed map field access in generated code.

## Remaining Enhancements
- [ ] Improve generated code formatting to more closely match `tests/human/x/dart` examples.
