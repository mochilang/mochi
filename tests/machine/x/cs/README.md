# C# Machine Output

This directory contains C# source generated from the Mochi programs in `tests/vm/valid`.
Each program has a `.cs` file along with the expected output `.out` if compilation
and execution succeeded. If a program failed to compile or run, an `.error` file
is present instead.

Compiled programs: 96/100

Failing programs:
- cast_string_to_int.mochi
- cross_join.mochi
- dataset_sort_take_limit.mochi
- dataset_where_filter.mochi

The remaining programs compile and run successfully under `dotnet`.
