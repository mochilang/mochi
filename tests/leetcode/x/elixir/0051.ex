defmodule Main do
  def solve(n) do
    dfs(0, n, MapSet.new(), MapSet.new(), MapSet.new(), [], []) |> Enum.reverse()
  end
  defp dfs(r, n, _cols, _d1, _d2, board, acc) when r == n, do: [Enum.reverse(board) | acc]
  defp dfs(r, n, cols, d1, d2, board, acc) do
    Enum.reduce(0..(n - 1), acc, fn c, acc ->
      a = r + c
      b = r - c + n - 1
      if MapSet.member?(cols, c) or MapSet.member?(d1, a) or MapSet.member?(d2, b) do
        acc
      else
        row = String.duplicate(".", c) <> "Q" <> String.duplicate(".", n - c - 1)
        dfs(r + 1, n, MapSet.put(cols, c), MapSet.put(d1, a), MapSet.put(d2, b), [row | board], acc)
      end
    end)
  end
  def main do
    lines = IO.read(:eof) |> String.split("
", trim: true)
    case lines do
      [] -> :ok
      [tstr | rest] ->
        {t, _} = Integer.parse(String.trim(tstr))
        {out, _} = Enum.reduce(1..t, {[], rest}, fn tc, {acc, xs} ->
          [nstr | xs1] = xs
          {n, _} = Integer.parse(String.trim(nstr))
          sols = solve(n)
          block = [Integer.to_string(length(sols))] ++ Enum.flat_map(Enum.with_index(sols), fn {sol, si} -> sol ++ if si + 1 < length(sols), do: ["-"], else: [] end) ++ if tc < t, do: ["="], else: []
          {acc ++ block, xs1}
        end)
        IO.write(Enum.join(out, "
"))
    end
  end
end
Main.main()
