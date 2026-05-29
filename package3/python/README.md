# package3/python

Bidirectional Python package bridge for Mochi. Implementation lives under this directory once phases land per [MEP-71 implementation tracking](../../website/docs/implementation/0071/index.md).

## Status

Phase 0 LANDED (2026-05-29). `Driver`, `Venv`, `SkipReason`, `BridgeError`, and the deterministic pyproject.toml renderer are in place. Phases 1-18 NOT STARTED.

## What this is

The MEP-71 bridge sits between MEP-51 (Mochi-to-Python transpiler) and MEP-57 (Mochi source-level package system). Two directions:

- **Consume**: `import python "<package>@<semver>" as <alias>` in Mochi source. The bridge resolves the package via uv, ingests PEP 561 stubs (4-tier precedence: `py.typed` inline, sibling `<name>-stubs`, typeshed, stubgen fallback), lowers via a closed type table, synthesises a CPython extension wrapper module, and exposes Python items as Mochi `extern fn` declarations.
- **Publish**: `mochi pkg publish --to=pypi`. The bridge lowers the Mochi package via a new `TargetPythonPackage` (sdist + wheel), runs the `mochi-build` PEP 517 backend, and uploads through PyPI Trusted Publishing (Sigstore-keyless OIDC) with PEP 740 attestations.

## Planned layout

```
package3/python/
  errors/         # SkipReason + BridgeError (phase 0)
  build/          # Workspace + Driver + Venv + libpython link (phase 0)
  semver/         # PEP 440 version parser (phase 1)
  simple/         # PEP 503 / 691 / 700 simple-index client (phase 1)
  uv/             # uv subprocess bridge + uv.lock + PEP 751 pylock.toml (phase 2)
  stubs/          # PEP 561 stub discovery + stubgen sandbox + .pyi parser (phase 3)
  typemap/        # closed type table + SkipReport (phase 4)
  wrapper/        # CPython extension synthesiser (phase 5)
  emit/           # Mochi extern-fn emitter (phase 6)
  publish/        # PyPI publish + Sigstore + PEP 740 (phase 11)
  attest/         # attestation verification (phase 15)
  pyodide/        # wasm32-emscripten + WASI Preview 2 (phase 16)
  freethread/     # free-threaded mode wrapper variants (phase 17)
  runtime/        # the embedded mochi_runtime Python package (phase 5 + phase 12)
```

## References

- [MEP-71 spec](../../website/docs/mep/mep-0071.md) for the normative design.
- [MEP-71 research bundle](../../website/docs/research/0071/index.md) for the 12-note deep-research collection.
- [MEP-71 implementation tracking](../../website/docs/implementation/0071/index.md) for the 18-phase rollout.
