# Sandbox Dockerfiles

This tool generates simple Dockerfiles for the programming languages used by Mochi.
Languages are discovered from the `compile` and `compile/x` directories.

The generated Dockerfiles are checked into git. If a new compiler is added,
re-run the generator from the repository root:

```bash
go run ./tools/sandbox/cmd/gensandbox
```

Dockerfiles are written to `tools/sandbox/dockerfiles/<lang>.Dockerfile`.
Each Dockerfile builds a small Go "runner" binary using a multi-stage build.
The runner implements a JSON protocol for running code inside the container.
See `LANGUAGE_RUNNER_PROTOCOL.md` for details.
