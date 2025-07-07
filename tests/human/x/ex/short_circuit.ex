def boom(a, b) do
  IO.puts("boom")
  true
end

IO.inspect(false and boom(1, 2))
IO.inspect(true or boom(1, 2))
