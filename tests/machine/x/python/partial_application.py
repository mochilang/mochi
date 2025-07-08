from functools import partial

def add(a, b):
    return a + b
add5 = partial(add, 5)
print(add5(3))
