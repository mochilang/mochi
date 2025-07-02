# SQLLogicTest Conversion

This tool downloads SQLLogicTest scripts and converts the statements and queries
into small Mochi programs. The generated programs live under
`tests/dataset/slt/out/<test-name>` together with the expected output.

Run the tool from the repository root:

```bash
go run ./cmd/mochi-slt fetch
go run ./cmd/mochi-slt gen
```

The first execution downloads the `.test` files from the Greg Rahn
`sqllogictest` repository. Subsequent runs reuse the local copies.
