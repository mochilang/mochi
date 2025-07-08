xs = [1, 2, 3]
ys = Enum.filter(xs, fn x -> rem(x, 2) == 1 end)

IO.inspect(1 in ys)
IO.inspect(2 in ys)

m = %{a: 1}
IO.inspect(Map.has_key?(m, :a))
IO.inspect(Map.has_key?(m, :b))

s = "hello"
IO.inspect(String.contains?(s, "ell"))
IO.inspect(String.contains?(s, "foo"))
