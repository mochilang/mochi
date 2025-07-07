class Counter:
    def __init__(self, n: int):
        self.n = n

def inc(c: Counter):
    c.n = c.n + 1

c = Counter(0)
inc(c)
print(c.n)
