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

## Language Runner Server Architecture

Each sandbox container embeds a small Go binary called the **runner**. The
runner reads a single JSON request on `stdin`, executes the requested program or
action inside the container, and writes a JSON response back on `stdout`. This
interface is referred to as the **Language Runner Protocol (LRP)**.

At a high level the architecture looks like this:

```
┌────────────┐    JSON over stdio    ┌────────────┐
│ client CLI │  ───────────────────▶ │   runner   │
└────────────┘  ◀─────────────────── │  (sandbox) │
                                      └────────────┘
```

1. The client (for example `mochi run` or an editor integration) sends a JSON
   message describing the action to perform and any source files.
2. The runner writes files to disk, invokes the language toolchain, captures the
   output and returns the result as JSON.
3. Different language images reuse the same runner binary but provide language
   specific compilers and runtimes.

This protocol is intentionally simple so that new languages can be added by
creating a Dockerfile and minimal Go stub under `runner/<lang>`.

## Planned Language Runner Protocol Features

The following checklist tracks the major features inspired by the Language
Server Protocol. They are not yet implemented but outline the intended
capabilities of the runner service.

### Lifecycle and Meta

* [ ] `initialize` – start the session and return capabilities
* [ ] `shutdown` – gracefully shut down the server
* [ ] `exit` – terminate the process after shutdown
* [ ] `runner/ping` – lightweight health check
* [ ] `runner/info` – return runtime metadata

### Program Execution

* [ ] `runner/run` – execute a source file with optional arguments
* [ ] `runner/repl/start` – launch an interactive REPL session
* [ ] `runner/repl/stop` – terminate the REPL session
* [ ] `runner/repl/send` – send code to the REPL and return output
* [ ] `runner/exec` – run a sandboxed shell command

### Build and Test

* [ ] `runner/test` – run the project's test suite
* [ ] `runner/build` – compile or build the project
* [ ] `runner/clean` – remove build artifacts
* [ ] `runner/package` – create a distributable artifact

### Introspection and Environment

* [ ] `runner/inspect` – inspect the runtime environment
* [ ] `runner/filesystem` – list accessible files
* [ ] `runner/deps` – list declared dependencies
* [ ] `runner/metadata` – return project metadata

### Developer Tools

* [ ] `runner/lint` – run the language linter
* [ ] `runner/format` – format source code
* [ ] `runner/docs` – generate documentation
* [ ] `runner/types` – return type information
* [ ] `runner/outline` – structural code outline

### Security and Sandbox Control

* [ ] `runner/limits` – report current runtime limits
* [ ] `runner/configure` – update environment configuration
* [ ] `runner/snapshot` – capture a sandbox snapshot
* [ ] `runner/restore` – restore from a snapshot

### File and Project Management

* [ ] `runner/fs/readFile` – read a file from the project
* [ ] `runner/fs/writeFile` – write a file to the project
* [ ] `runner/fs/listDir` – list files under a path
* [ ] `runner/fs/delete` – delete a file or directory
* [ ] `runner/project/init` – initialize a new project
