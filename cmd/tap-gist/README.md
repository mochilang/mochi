# tap-gist

Create a GitHub gist from one or more files or from standard input.

```bash
tap-gist [-m message] [--public] [--token TOKEN] <file|-> [file ...]
```

- `-m` sets the gist description.
- `--public` creates a public gist (private by default).
- `--token` overrides the GitHub token (defaults to the `GITHUB_TOKEN` environment variable).

The `GITHUB_TOKEN` variable is already set in this repository.
