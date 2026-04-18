defmodule Main do
  def solve(s1, s2), do: dfs(s1, s2, 0, 0, String.length(s1), %{}) |> elem(0)

  defp dfs(s1, s2, i1, i2, len, memo) do
    key = {i1, i2, len}
    case memo do
      %{^key => v} -> {v, memo}
      _ ->
        a = String.slice(s1, i1, len)
        b = String.slice(s2, i2, len)
        cond do
          a == b -> {true, Map.put(memo, key, true)}
          Enum.sort(String.graphemes(a)) != Enum.sort(String.graphemes(b)) -> {false, Map.put(memo, key, false)}
          true -> try_splits(s1, s2, i1, i2, len, 1, memo, key)
        end
    end
  end

  defp try_splits(_s1, _s2, _i1, _i2, len, k, memo, key) when k >= len, do: {false, Map.put(memo, key, false)}
  defp try_splits(s1, s2, i1, i2, len, k, memo, key) do
    {a1, memo} = dfs(s1, s2, i1, i2, k, memo)
    {a2, memo} = dfs(s1, s2, i1 + k, i2 + k, len - k, memo)
    if a1 and a2 do
      {true, Map.put(memo, key, true)}
    else
      {b1, memo} = dfs(s1, s2, i1, i2 + len - k, k, memo)
      {b2, memo} = dfs(s1, s2, i1 + k, i2, len - k, memo)
      if b1 and b2, do: {true, Map.put(memo, key, true)}, else: try_splits(s1, s2, i1, i2, len, k + 1, memo, key)
    end
  end

  def main do
    lines = IO.read(:all) |> String.split(~r/\r?\n/, trim: false)
    if lines != [] and String.trim(Enum.at(lines, 0)) != "" do
      {t, _} = Integer.parse(String.trim(Enum.at(lines, 0)))
      out = Enum.map(0..(t - 1), fn i -> if solve(Enum.at(lines, 1 + 2 * i), Enum.at(lines, 2 + 2 * i)), do: "true", else: "false" end)
      IO.write(Enum.join(out, "\n"))
    end
  end
end

Main.main()
