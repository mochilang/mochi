defmodule Main do
  def hist(arr) do
    t = List.to_tuple(arr)
    n = tuple_size(t)
    outer(0, n, t, 0)
  end

  defp outer(i, n, _t, best) when i >= n, do: best
  defp outer(i, n, t, best), do: outer(i + 1, n, t, inner(i, i, n, t, elem(t, i), best))

  defp inner(_i, j, n, _t, _mn, best) when j >= n, do: best
  defp inner(i, j, n, t, mn, best) do
    v = elem(t, j)
    mn2 = if v < mn, do: v, else: mn
    area = mn2 * (j - i + 1)
    best2 = if area > best, do: area, else: best
    inner(i, j + 1, n, t, mn2, best2)
  end

  def main do
    toks = IO.read(:all) |> String.split(~r/\s+/, trim: true)
    if toks != [] do
      {t, _} = Integer.parse(Enum.at(toks, 0))
      {_, out} =
        Enum.reduce(0..(t - 1), {1, []}, fn _, {idx, out} ->
          {rows, _} = Integer.parse(Enum.at(toks, idx))
          {cols, _} = Integer.parse(Enum.at(toks, idx + 1))
          {_, _, best} =
            Enum.reduce(0..(rows - 1), {idx + 2, List.duplicate(0, cols), 0}, fn _, {i2, h, best} ->
              s = Enum.at(toks, i2)
              h2 = Enum.map(0..(cols - 1), fn c -> if String.at(s, c) == "1", do: Enum.at(h, c) + 1, else: 0 end)
              {i2 + 1, h2, max(best, hist(h2))}
            end)
          {idx + 2 + rows, out ++ [Integer.to_string(best)]}
        end)
      IO.write(Enum.join(out, "\n"))
    end
  end
end

Main.main()
