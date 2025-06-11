# libmochi

Python helpers for executing Mochi code.

The `run` function executes a string of Mochi source using the `mochi` binary
and returns its standard output. `call` lets you define a function in Mochi code
and invoke it while decoding the JSON result.  The `eval` helper does not rely
on the binary. Instead it launches a small Go program through `go run` to
interpret the code and returns the captured output.

```
pip install -e tools/libmochi/python
```

```python
from libmochi import run, call, eval

print(run('print("hello")'))
print(call('fun add(a:int, b:int): int { return a + b }', 'add', 2, 3))
print(eval('print("ffi")'))
```

Both helpers expect the `mochi` binary to be available on your `PATH`.
