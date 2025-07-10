from __future__ import annotations


def boom(a: int, b: int) -> bool:
    print("boom")
    return True


print(str(False and boom(1, 2)).lower())
print(str(True or boom(1, 2)).lower())
