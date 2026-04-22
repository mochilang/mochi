defmodule Main do
  def max_profit(prices) do
    {_, sell1, _, sell2} =
      Enum.reduce(prices, {-1_000_000_000, 0, -1_000_000_000, 0}, fn p, {buy1, sell1, buy2, sell2} ->
        buy1 = max(buy1, -p)
        sell1 = max(sell1, buy1 + p)
        buy2 = max(buy2, sell1 - p)
        sell2 = max(sell2, buy2 + p)
        {buy1, sell1, buy2, sell2}
      end)

    sell2
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
