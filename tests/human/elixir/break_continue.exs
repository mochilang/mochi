numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9]

Enum.reduce_while(numbers, nil, fn n, _ ->
  cond do
    rem(n, 2) == 0 -> {:cont, nil}
    n > 7 -> {:halt, nil}
    true ->
      IO.puts("odd number: #{n}")
      {:cont, nil}
  end
end)
