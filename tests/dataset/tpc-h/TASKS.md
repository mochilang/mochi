# TPCH Test Notes

Each query in this directory has a `.mochi` implementation with inline data and a test block verifying the expected result. Earlier issues around rounding and parsing in a few programs (q8, q13, q21 and q22) have been resolved. If a test fails, re-run the query and compare the printed output with the expected result file.

## Current failing queries

Running `TestVM_TPCH` across `q1.mochi` to `q22.mochi` now succeeds for every program except `q22`. All other queries execute correctly using the runtime/vm and match their expected output. Query `q22` still fails to parse and requires investigation. Once fixed, the TPCH suite will have full coverage under the VM runtime.
