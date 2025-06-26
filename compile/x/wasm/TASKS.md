# WebAssembly Backend Tasks for TPCH Q1

The WebAssembly backend translates Go-generated code into WASM modules, but dataset queries are only partially supported.

- Lower dataset loops to explicit WASM instructions that build groups in linear memory.
- Provide imported functions implementing `group_by` and aggregate helpers.
- Represent structs in linear memory with offsets accessible from the runtime.
- Serialize the result back to JSON in the host environment and add tests under `tests/compiler/wasm`.
