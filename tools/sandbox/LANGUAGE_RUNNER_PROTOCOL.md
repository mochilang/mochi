# Language Runner Protocol

Containers built from the sandbox Dockerfiles expose a `runner` binary as their
entrypoint. The runner accepts a single JSON message on `stdin` describing the
files that should be executed. It writes a JSON response on `stdout` with the
captured program output.

## Request

```json
{
  "files": {
    "filename": "source code contents"
  }
}
```

`files` is a map of file names to their contents. The runner writes these files
to the working directory and executes the language-specific command.

## Response

```json
{
  "output": "stdout and stderr combined",
  "error": "error message if the command failed"
}
```

If execution succeeds, `error` will be omitted.
