"""Synchronous broadcast stream primitives for the Phase 10.0 surface.

Mochi v1 streams are MPMC broadcast: each subscriber sees every value
emitted after it subscribed. Phase 10.0 supports the single-threaded
synchronous case (emit and recv interleaved on the same execution path),
mirroring the existing C-fixture corpus at tests/transpiler3/c/fixtures/
stream/. Async / cross-task broadcast is Phase 11+.

Buffer growth: each MochiStream holds an append-only list and each
MochiSub holds a read cursor. recv_sub advances the cursor by one.
Cap is recorded but not enforced (no producer-side backpressure in
the sync surface). Callers that need bounded broadcast use
asyncio.Queue under the Phase 11+ async surface.
"""

from __future__ import annotations

from typing import Generic, TypeVar

T = TypeVar("T")


class MochiStream(Generic[T]):
    __slots__ = ("_buffer", "_cap")

    def __init__(self, cap: int) -> None:
        self._buffer: list[T] = []
        self._cap = cap


class MochiSub(Generic[T]):
    __slots__ = ("_stream", "_pos")

    def __init__(self, stream: MochiStream[T]) -> None:
        self._stream = stream
        self._pos = len(stream._buffer)


def mochi_make_stream(cap: int) -> MochiStream[T]:
    return MochiStream(cap)


def mochi_subscribe(stream: MochiStream[T]) -> MochiSub[T]:
    return MochiSub(stream)


def mochi_emit(stream: MochiStream[T], val: T) -> None:
    stream._buffer.append(val)


def mochi_recv_sub(sub: MochiSub[T]) -> T:
    v = sub._stream._buffer[sub._pos]
    sub._pos += 1
    return v
