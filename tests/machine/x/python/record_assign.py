from dataclasses import dataclass

@dataclass
class Counter:
    n: int

def inc(c):
    c.n = c.n + 1
c = Counter(n=0)
inc(c)
print(c.n)
