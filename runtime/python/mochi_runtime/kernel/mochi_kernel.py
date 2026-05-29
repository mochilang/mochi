"""MochiKernel: ipykernel subclass that transpiles each cell on
receipt.

The cell text submitted by JupyterLab is written to a temporary
`.mochi` file, fed to the Mochi binary (`mochi build
--target=python-source`), and the emitted `generated/<module>.py`
is then exec'd into the IPython shell's `user_ns` so subsequent
cells observe variable bindings, function definitions, imports,
and side effects.

Subprocess-based transpile keeps the kernel out of the Mochi Go
binary's CPython embedding story. The cost is per-cell latency of
~30ms cold + ~10ms warm; acceptable for interactive use.

The Mochi binary is located via, in priority order:

    1. `$MOCHI_BIN` env var (test harness uses this to point at a
       `go run` invocation or a local build).
    2. `mochi` on PATH.

A missing binary surfaces as an `ipykernel` error stream containing
the resolution failure; the cell is not silently dropped.
"""

from __future__ import annotations

import os
import shlex
import shutil
import subprocess
import tempfile
from pathlib import Path
from typing import Any

from ipykernel.ipkernel import IPythonKernel


class MochiKernelError(RuntimeError):
    """Raised when the Mochi binary cannot be located or fails to
    transpile a cell. The kernel converts this to an `ipykernel`
    error stream and returns an `execute_reply` with status="error"
    so the JupyterLab front-end shows the message inline."""


class MochiKernel(IPythonKernel):
    implementation = "mochi"
    implementation_version = "0.1.0"
    language = "mochi"
    language_version = "0.1.0"
    language_info = {
        "name": "mochi",
        "mimetype": "text/x-mochi",
        "file_extension": ".mochi",
        "pygments_lexer": "mochi",
    }
    banner = "Mochi 0.1 (MEP-51 transpile-to-python)"

    def __init__(self, **kwargs: Any) -> None:
        super().__init__(**kwargs)
        self._cell_counter = 0

    def do_execute(  # type: ignore[override]
        self,
        code: str,
        silent: bool,
        store_history: bool = True,
        user_expressions: dict[str, Any] | None = None,
        allow_stdin: bool = False,
        *,
        cell_id: str | None = None,
    ) -> dict[str, Any]:
        self._cell_counter += 1
        try:
            py_source = self._transpile_cell(code)
        except MochiKernelError as exc:
            if not silent:
                self.send_response(
                    self.iopub_socket,
                    "stream",
                    {"name": "stderr", "text": str(exc) + "\n"},
                )
            return {
                "status": "error",
                "execution_count": self.execution_count,
                "ename": type(exc).__name__,
                "evalue": str(exc),
                "traceback": [str(exc)],
            }

        return super().do_execute(
            py_source,
            silent,
            store_history=store_history,
            user_expressions=user_expressions,
            allow_stdin=allow_stdin,
            cell_id=cell_id,
        )

    @classmethod
    def _resolve_mochi_bin(cls) -> list[str]:
        """Return the argv prefix that invokes the Mochi binary.

        `MOCHI_BIN` may be either a path to a built binary or a full
        command line (e.g. `go run ./cmd/mochi`) parsed via
        `shlex.split`. This matches how CI and local test runs invoke
        the kernel without first installing the Mochi binary.
        """
        env = os.environ.get("MOCHI_BIN", "").strip()
        if env:
            return shlex.split(env)
        path = shutil.which("mochi")
        if path:
            return [path]
        raise MochiKernelError(
            "mochi binary not found: set MOCHI_BIN or add `mochi` to PATH"
        )

    @classmethod
    def _transpile_cell(cls, mochi_source: str) -> str:
        """Round-trip mochi_source through the Mochi binary's Python
        source target. Returns the generated Python text suitable for
        exec'ing into the IPython namespace.

        Cell mode: top-level statements without a `main` wrapper are
        accepted; the source is wrapped in a `main:` block written to
        a temp file, transpiled, and the resulting Python's body is
        extracted by stripping the `def main()` wrapper. This keeps
        the kernel decoupled from a separate Mochi `--mode=cell`
        flag; once Phase 17.x adds one, the wrapper trick can be
        removed.
        """
        argv = cls._resolve_mochi_bin()
        with tempfile.TemporaryDirectory(prefix="mochi-kernel-cell-") as tmp:
            tmp_path = Path(tmp)
            src = tmp_path / "cell.mochi"
            out = tmp_path / "out"
            src.write_text(mochi_source, encoding="utf-8")
            cmd = argv + [
                "build",
                "--target",
                "python-source",
                "--out",
                str(out),
                str(src),
            ]
            proc = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                check=False,
            )
            if proc.returncode != 0:
                raise MochiKernelError(
                    f"mochi build failed: {proc.stderr.strip() or proc.stdout.strip()}"
                )
            # The Phase 1 layout emits the generated body to
            # src/<pkg>/generated/<module>.py. Walk the out tree to
            # find it without hard-coding the package prefix (which
            # the driver derives from the source filename).
            gen_files = list(out.glob("src/*/generated/*.py"))
            gen_files = [p for p in gen_files if p.name != "__init__.py"]
            if not gen_files:
                raise MochiKernelError(
                    f"mochi build produced no generated/<module>.py under {out}"
                )
            return cls._unwrap_main(gen_files[0].read_text(encoding="utf-8"))

    @staticmethod
    def _unwrap_main(source: str) -> str:
        """Strip the `def main()` wrapper and `if __name__` trailer
        from cell-transpiled output.

        The Phase 1 source target wraps cell bodies in
        `def main() -> None: ...` and tails them with
        `if __name__ == "__main__":\\n    main()`. For Jupyter cell
        semantics we need the body's bindings to land at module
        scope so the next cell observes them via `user_ns`. The
        transform keeps top-level prelude (imports, dataclasses),
        dedents the `def main` body in place, and drops the
        `if __name__` trailer because we have already inlined the
        main body.
        """
        lines = source.splitlines(keepends=True)
        out: list[str] = []
        i = 0
        n = len(lines)
        while i < n:
            line = lines[i]
            stripped = line.lstrip()
            if stripped.startswith("def main(") or stripped.startswith("def main "):
                i += 1
                body_indent = ""
                # Pull out the indented body, dedent it.
                while i < n:
                    bl = lines[i]
                    if bl.strip() == "":
                        out.append("\n")
                        i += 1
                        continue
                    if not body_indent:
                        body_indent = bl[: len(bl) - len(bl.lstrip())]
                        if not body_indent:
                            break
                    if bl.startswith(body_indent):
                        out.append(bl[len(body_indent):])
                        i += 1
                    else:
                        break
                continue
            if stripped.startswith("if __name__"):
                # Drop the trailer plus its indented body. Anything
                # at column zero after the indented block resumes
                # the top-level walk.
                i += 1
                while i < n and (lines[i].startswith((" ", "\t")) or lines[i].strip() == ""):
                    i += 1
                continue
            out.append(line)
            i += 1
        return "".join(out)
