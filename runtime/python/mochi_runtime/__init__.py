"""Mochi-to-Python runtime support package.

MEP-51 Phase 1 ships only the `io.Print` printer; later phases add
agents (asyncio.Queue), streams (AsyncIterator), MochiResult, and
FFI wrappers.
"""

from __future__ import annotations

from .io import Print

__all__ = ["Print"]
__version__ = "0.1.0"
