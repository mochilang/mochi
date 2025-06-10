# Mochi Jupyter Extension

This folder provides a small IPython extension that lets you run Mochi code
inside JupyterLab notebooks using a `%%mochi` cell magic.

## Usage

1. Make sure the `mochi` binary is on your `PATH`.
   You can build it with `make build` from the repository root or download a
   release from GitHub.
2. Add `tools/notebook` to your `PYTHONPATH` or install it with `pip`:

```bash
pip install -e tools/notebook
```

3. Load the extension in a notebook:

```python
%load_ext mochi_magic
```

Now any cell starting with `%%mochi` will be executed by the Mochi CLI:

```mochi
%%mochi
print("hello from Mochi")
```

The cell output appears below just like regular Python code.
