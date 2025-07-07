from functools import partial

def add(a: int, b: int) -> int:
    return a + b

add5 = partial(add, 5)
print(add5(3))
