# TPCH Test Notes

Each query in this directory has a `.mochi` implementation with inline data and a test block verifying the expected result. Earlier issues around rounding and parsing in a few programs (q8, q13, q21 and q22) have been resolved. If a test fails, re-run the query and compare the printed output with the expected result file.

## Current failing queries

Running `TestVM_TPCH` across `q1.mochi` to `q22.mochi` now succeeds for all queries except for the following:

* `q21` – fails an `expect` condition
* `q22` – parse error

Addressing these two failures will provide full TPCH coverage under the VM runtime.
