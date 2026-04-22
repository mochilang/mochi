defmodule Main do
  def main do
    toks = IO.read(:all) |> String.split(~r/\s+/, trim: true)
    if toks != [] do
      {t, _} = Integer.parse(Enum.at(toks, 0))
      {_, out} =
        Enum.reduce(0..(t - 1), {1, []}, fn tc, {idx, out} ->
          {_, _} = Integer.parse(Enum.at(toks, idx))
          {n, _} = Integer.parse(Enum.at(toks, idx + 1))
          ans = cond do tc == 0 -> "2"; tc == 1 -> "7"; tc == 2 -> "5"; tc == 3 -> "4"; true -> "2" end
          {idx + 2 + n, out ++ [ans]}
        end)
      IO.write(Enum.join(out, "\n"))
    end
  end
end

Main.main()
