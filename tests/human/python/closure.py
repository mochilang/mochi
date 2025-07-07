# Translation of closure.mochi to Python

def makeAdder(n):
    def adder(x):
        return x + n
    return adder

add10 = makeAdder(10)
print(add10(7))  # 17
