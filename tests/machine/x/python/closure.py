from __future__ import annotations
import typing


def makeAdder(n: int) -> typing.Callable[[int], int]:
    return lambda x: x + n


add10 = makeAdder(10)
print(add10(7))
