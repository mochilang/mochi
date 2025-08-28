nums = [1, 2, 3]
letters = ["A", "B"]

pairs = for n <- nums, l <- letters, rem(n, 2) == 0 do
  %{n: n, l: l}
end

IO.puts("--- Even pairs ---")
Enum.each(pairs, fn p ->
  IO.puts("#{p.n} #{p.l}")
end)
