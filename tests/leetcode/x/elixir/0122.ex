defmodule Main do
  def max_profit(prices), do: do_max(prices, 0)

  defp do_max([a, b | rest], best) when b > a, do: do_max([b | rest], best + b - a)
  defp do_max([_a, b | rest], best), do: do_max([b | rest], best)
  defp do_max(_, best), do: best

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
