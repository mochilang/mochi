defmodule Main do
  def solve(arr) do
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
          {n, _} = Integer.parse(Enum.at(toks, idx))
          arr =
            Enum.map((idx + 1)..(idx + n), fn k ->
              {v, _} = Integer.parse(Enum.at(toks, k))
              v
            end)
          {idx + n + 1, out ++ [Integer.to_string(solve(arr))]}
        end)
      IO.write(Enum.join(out, "\n"))
    end
  end
end

Main.main()
