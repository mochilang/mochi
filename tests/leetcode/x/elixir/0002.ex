defmodule Main do
  def add_lists(a, b), do: do_add(a, b, 0, [])
  defp do_add([], [], 0, acc), do: Enum.reverse(acc)
  defp do_add([], [], carry, acc), do: Enum.reverse([carry | acc])
  defp do_add([x | xs], [], carry, acc) do
    sum = x + carry
    do_add(xs, [], div(sum, 10), [rem(sum, 10) | acc])
  end
  defp do_add([], [y | ys], carry, acc) do
    sum = y + carry
    do_add([], ys, div(sum, 10), [rem(sum, 10) | acc])
  end
  defp do_add([x | xs], [y | ys], carry, acc) do
    sum = x + y + carry
    do_add(xs, ys, div(sum, 10), [rem(sum, 10) | acc])
  end
  defp fmt(arr), do: "[" <> Enum.join(Enum.map(arr, &Integer.to_string/1), ",") <> "]"
  def run do
    tokens = IO.read(:eof) |> String.split(~r/\s+/, trim: true)
    if tokens != [] do
      {t, _} = Integer.parse(hd(tokens))
      {_idx, lines} = Enum.reduce(1..t, {1, []}, fn _, {idx, acc} ->
        {n, _} = Integer.parse(Enum.at(tokens, idx))
        a = Enum.map(Enum.slice(tokens, idx + 1, n), &String.to_integer/1)
        idx2 = idx + 1 + n
        {m, _} = Integer.parse(Enum.at(tokens, idx2))
        b = Enum.map(Enum.slice(tokens, idx2 + 1, m), &String.to_integer/1)
        {idx2 + 1 + m, acc ++ [fmt(add_lists(a, b))]}
      end)
      IO.write(Enum.join(lines, "\n"))
    end
  end
end

Main.run()
