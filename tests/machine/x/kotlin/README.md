# Kotlin Machine Outputs

This directory contains Kotlin source files generated from the Mochi programs in
`tests/vm/valid`. They are produced by the Kotlin backend in
`compiler/x/kotlin` and compiled using `kotlinc`.

## Compilation status

As of this commit the test suite compiles and runs most example programs. The
following 13 programs currently fail to compile and have corresponding `.error`
files:

- group_by_conditional_sum
- group_by_join
- group_by_left_join
- group_by_multi_join_sort
- group_by_sort
- group_items_iteration
- in_operator_extended
- order_by_map
- outer_join
- query_sum_select
- right_join
- sort_stable
- tree_sum

Successful programs have matching `.kt` and `.out` files.

## TODO

- Implement dataset join and group-by operations in the Kotlin backend.
- Provide better support for the Python `math` helpers and other foreign
  imports.
- Finish compiling the remaining programs listed above.
