def sum_rec(0, acc), do: acc
def sum_rec(n, acc), do: sum_rec(n - 1, acc + n)

IO.inspect(sum_rec(10, 0))
