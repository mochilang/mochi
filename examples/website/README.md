Source for the six code samples shown on https://mochi-lang.dev.

Each `.mochi` file matches a tab in the homepage `CodeShowcase`. They are
runnable end to end with the published `mochi` binary, and exist here so
that homepage edits can be regression-tested against the real toolchain.

```
mochi run hello.mochi          # Sample 1: Hello, Mochi
mochi run shapes.mochi         # Sample 2: Types and functions
mochi run events.mochi         # Sample 3: Tagged unions
mochi run summarize.mochi      # Sample 4: Generative AI (echo provider)
mochi run top-products.mochi   # Sample 5: Datasets (reads products.json)
mochi test math.mochi          # Sample 6: Tests
```

When the homepage in `website/src/pages/index.js` changes, update the
matching file here and re-run the snippets to confirm they still execute.
