# tap-gist

`tap-gist` uploads files or standard input to a GitHub Gist and prints the resulting URL.

## Usage

Ensure `GITHUB_TOKEN` is set in your environment with a token that has gist
creation permissions. The command will read it automatically.

```
tap-gist [flags] [file ...]
```

If no files are provided, `tap-gist` reads from standard input. When reading from
stdin the file name defaults to `stdin.txt` and can be changed with `--name`.

### Flags

- `--desc`  description for the gist
- `--public`  create a public gist
- `--name`  filename when reading from stdin

### Examples

Upload the contents of `example.txt`:

```bash
tap-gist example.txt
```

Pipe from another command:

```bash
echo "hello" | tap-gist --desc "demo"
```

