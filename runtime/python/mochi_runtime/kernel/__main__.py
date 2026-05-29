"""Entry point: `python -m mochi_runtime.kernel`.

The kernelspec emitted by `mochi build --target=python-ipykernel`
points `argv` at this module so Jupyter launches the kernel via the
standard `IPKernelApp.launch_instance`. The MochiKernel class
overrides `do_execute` to insert the Mochi-to-Python transpile step.
"""

from __future__ import annotations

from ipykernel.kernelapp import IPKernelApp

from .mochi_kernel import MochiKernel


def main() -> None:
    IPKernelApp.launch_instance(kernel_class=MochiKernel)


if __name__ == "__main__":
    main()
