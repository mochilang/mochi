defmodule Main do
  def trap(h), do: go(h, 0, length(h) - 1, 0, 0, 0)
  defp go(_h, left, right, _lm, _rm, water) when left > right, do: water
  defp go(h, left, right, lm, rm, water) do
    if lm <= rm do
      v = Enum.at(h, left)
      if v < lm, do: go(h, left + 1, right, lm, rm, water + lm - v), else: go(h, left + 1, right, v, rm, water)
    else
      v = Enum.at(h, right)
      if v < rm, do: go(h, left, right - 1, lm, rm, water + rm - v), else: go(h, left, right - 1, lm, v, water)
    end
  end
  def main do
    lines = IO.read(:eof) |> String.split("
", trim: true)
    case lines do
      [] -> :ok
      [tstr | rest] ->
        {t, _} = Integer.parse(String.trim(tstr))
        {out, _} = Enum.reduce(1..t, {[], rest}, fn _, {acc, xs} ->
          [nstr | xs] = xs
          {n, _} = Integer.parse(String.trim(nstr))
          {vals, xs} = Enum.split(xs, n)
          arr = Enum.map(vals, fn s -> elem(Integer.parse(String.trim(s)), 0) end)
          {[Integer.to_string(trap(arr)) | acc], xs}
        end)
        IO.write(Enum.reverse(out) |> Enum.join("\n"))
    end
  end
end
Main.main()
