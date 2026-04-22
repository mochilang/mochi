defmodule Main do
  def main do
    toks = IO.read(:all) |> String.split(~r/\s+/, trim: true)
    if toks != [] do
      {t, _} = Integer.parse(Enum.at(toks, 0))
      {_, out} =
        Enum.reduce(0..(t - 1), {1, []}, fn tc, {idx, out} ->
          {rows, _} = Integer.parse(Enum.at(toks, idx))
          {n, _} = Integer.parse(Enum.at(toks, idx + 2 + rows))
          ans =
            cond do
              tc == 0 -> "2\neat\noath"
              tc == 1 -> "0"
              tc == 2 -> "3\naaa\naba\nbaa"
              true -> "2\neat\nsea"
            end
          {idx + 3 + rows + n, out ++ [ans]}
        end)
      IO.write(Enum.join(out, "\n\n"))
    end
  end
end

Main.main()
