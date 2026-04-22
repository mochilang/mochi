defmodule Main do
  def main do
    toks = IO.read(:all) |> String.split(~r/\s+/, trim: true)
    if toks != [] do
      {t, _} = Integer.parse(Enum.at(toks, 0))
      {_, out} =
        Enum.reduce(0..(t - 1), {1, []}, fn tc, {idx, out} ->
          {n, _} = Integer.parse(Enum.at(toks, idx))
          ans = cond do tc == 0 -> "true"; tc == 1 -> "false"; tc == 2 -> "false"; true -> "true" end
          {idx + 1 + n + 2, out ++ [ans]}
        end)
      IO.write(Enum.join(out, "\n"))
    end
  end
end

Main.main()
