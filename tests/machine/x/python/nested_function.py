from __future__ import annotations


def outer(x: int) -> int:

    def inner(y: int) -> int:
        return x + y

    return inner(5)


print(outer(3))
