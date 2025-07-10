from __future__ import annotations
import dataclasses


def inc(c: Counter) -> None:
    c = c.n + 1


@dataclasses.dataclass
class Counter:
    n: int


c = Counter(n=0)
inc(c)
print(c.n)
