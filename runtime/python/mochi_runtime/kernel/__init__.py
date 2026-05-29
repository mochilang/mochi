"""Mochi Jupyter ipykernel package.

`mochi build --target=python-ipykernel` emits a kernelspec directory
that points Jupyter at this package via `python -m mochi_runtime.kernel`.
The package contains a single `MochiKernel` subclass of
`IPythonKernel` that intercepts `do_execute` to transpile each cell
from Mochi to Python before delegating to the inherited IPython
execution loop. Namespace persists across cells via the IPython
shell's `user_ns`, which gives Jupyter users the same cell-by-cell
semantics they expect from a Python kernel.
"""

from .mochi_kernel import MochiKernel

__all__ = ["MochiKernel"]
