numbers = [1,2,3,4,5,6,7,8,9]

Enum.reduce_while(numbers, :ok, fn n, _ ->
  cond do
    rem(n,2) == 0 ->
      {:cont, :ok}
    n > 7 ->
      {:halt, :ok}
    true ->
      IO.puts("odd number: #{n}")
      {:cont, :ok}
  end
end)
