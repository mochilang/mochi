# Tasks to enable running tpc-h/q1.mochi via Clojure compiler

The current Clojure backend fails to compile and execute `tpch_q1.mochi`. Issues observed:

- Generated Clojure code has unmatched delimiters due to missing closing `]` in `compileQueryHelper` when emitting grouped queries.
- Runtime helpers such as `_to_json` leave unbalanced parentheses leading to parser errors.
- Required dependencies like `clojure.data.json` are not included causing `ClassNotFoundException` in some tests.

To fully support tpc-h query execution the following tasks are recommended:

1. **Fix runtime helper definitions**
   - Review and correct all helper strings in `compile/x/clj/runtime.go` ensuring parentheses balance.
   - Add regression tests that load each helper into the Clojure reader.
2. **Close binding vector in grouped queries**
   - Ensure `compileQueryHelper` writes the closing `]` before the query body and that resulting code compiles.
3. **Add required namespaces**
   - Automatically require `clojure.data.json` and `org.yaml.snakeyaml` when helpers `_json`, `_load` or `_save` are used.
4. **Extend golden tests**
   - Include `tests/compiler/clj/tpch_q1.mochi` in the suite with Clojure installed to verify compilation and execution.
5. **CI environment setup**
   - Update test tooling to install Clojure and required libraries on Linux and macOS to prevent skipping tests.

Implementing these tasks will allow the Clojure compiler to run the tpc-h Q1 example and similar dataset queries.
