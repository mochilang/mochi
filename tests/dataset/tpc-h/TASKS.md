# TPCH Test Notes

Each query in this directory has a `.mochi` implementation with inline data and a test block verifying the expected result. Earlier issues around rounding and parsing in a few programs (q8, q13, q21 and q22) have been resolved. If a test fails, re-run the query and compare the printed output with the expected result file.

## Current failing queries

Running `TestVM_TPCH` across `q1.mochi` to `q22.mochi` highlighted a number of programs that still fail when executed using the runtime/vm. The following queries either produce incorrect output or encounter errors and should be investigated:

* `q2`
* `q4`
* `q5`
* `q7`
* `q9`
* `q10`
* `q14`
* `q15`
* `q16`
* `q19`
* `q20`
* `q21` – fails an `expect` condition
* `q22` – parse error

Resolving these issues will ensure full TPCH coverage under the VM runtime.
