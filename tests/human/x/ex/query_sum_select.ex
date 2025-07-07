nums = [1,2,3]
result =
  nums
  |> Enum.filter(&(&1 > 1))
  |> Enum.sum()
IO.puts(result)
