def makeAdder(n):
    return lambda x: x + n


add10 = makeAdder(10)
print(add10(7))
