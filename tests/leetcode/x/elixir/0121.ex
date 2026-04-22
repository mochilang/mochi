defmodule Main do
  def max_profit([]), do: 0
  def max_profit([first | rest]), do: do_max(rest, first, 0)

  defp do_max([], _min_price, best), do: best
  defp do_max([p | rest], min_price, best) do
    do_max(rest, min(min_price, p), max(best, p - min_price))
  end

  def main do
    lines = IO.read(:stdio, :eof) |> String.split(~r/\r?\n/, trim: true)
    if lines != [] do
      t = hd(lines) |> String.to_integer()
      {_, out} =
        Enum.reduce(1..t, {1, []}, fn _, {idx, out} ->
          n = Enum.at(lines, idx) |> String.to_integer()
          vals = Enum.map((idx + 1)..(idx + n), fn j -> Enum.at(lines, j) |> String.to_integer() end)
          {idx + n + 1, out ++ [Integer.to_string(max_profit(vals))]}
        end)

      IO.write(Enum.join(out, "\n"))
    end
  end
end

Main.main()
