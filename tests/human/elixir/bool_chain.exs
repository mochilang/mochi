boom = fn ->
  IO.puts("boom")
  true
end

IO.inspect((1 < 2) and (2 < 3) and (3 < 4))
IO.inspect((1 < 2) and (2 > 3) and boom.())
IO.inspect((1 < 2) and (2 < 3) and (3 > 4) and boom.())
