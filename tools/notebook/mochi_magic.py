import os
import sys
import tempfile
import subprocess
from IPython.core.magic import Magics, magics_class, cell_magic


@magics_class
class MochiMagics(Magics):
    """IPython cell magic to execute Mochi code."""

    @cell_magic
    def mochi(self, line, cell):
        """Run Mochi code contained in the cell."""
        with tempfile.NamedTemporaryFile("w", suffix=".mochi", delete=False) as f:
            f.write(cell)
            fname = f.name
        try:
            cmd = ["mochi", "run", fname]
            proc = subprocess.run(cmd, capture_output=True, text=True)
            if proc.stdout:
                sys.stdout.write(proc.stdout)
            if proc.stderr:
                sys.stderr.write(proc.stderr)
            if proc.returncode != 0:
                raise RuntimeError(f"mochi exited with status {proc.returncode}")
        finally:
            os.unlink(fname)


def load_ipython_extension(ip):
    ip.register_magics(MochiMagics)
