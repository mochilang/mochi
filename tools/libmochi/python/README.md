# libmochi

Python helpers for executing Mochi code.

The `run` function executes a string of Mochi source and returns its standard
output. `call` lets you define a function in Mochi code and invoke it while
decoding the JSON result.

```
pip install -e tools/libmochi/python
```

```python
from libmochi import run, call

print(run('print("hello")'))
print(call('fun add(a:int, b:int): int { return a + b }', 'add', 2, 3))
```

Both helpers expect the `mochi` binary to be available on your `PATH`.
