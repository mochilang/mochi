def sum_rec(n: int, acc: int) -> int:
    """sum_rec(n: int, acc: int) -> int"""
    if n == 0:
        return acc
    return sum_rec(n - 1, acc + n)


print(sum_rec(10, 0))
