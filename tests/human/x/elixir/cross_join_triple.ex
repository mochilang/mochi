nums = [1, 2]
letters = ["A", "B"]
bools = [true, false]

combos = for n <- nums, l <- letters, b <- bools do
  %{n: n, l: l, b: b}
end

IO.puts("--- Cross Join of three lists ---")
Enum.each(combos, fn c ->
  IO.puts("#{c.n} #{c.l} #{c.b}")
end)
