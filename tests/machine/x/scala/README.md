# Scala Machine Outputs

This directory contains Scala source files generated from the Mochi test suite.
Each program under `tests/vm/valid` is compiled to Scala and executed during the
`vm_golden_test` tests.  Successful runs have a `.out` file with the program
output.  When compilation or execution fails the error message is stored in a
matching `.error` file.

## Progress

The following Mochi programs compile and run successfully:

- `len_map.mochi`
- `map_nested_assign.mochi`
- `values_builtin.mochi`

Programs with a corresponding `.error` file currently fail to compile or run.
