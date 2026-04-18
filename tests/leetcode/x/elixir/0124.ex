defmodule Main do
  def solve(vals, ok) do
    {_gain, best} = dfs(0, vals, ok, -1_000_000_000)
    best
  end

  defp dfs(i, vals, ok, best) do
    cond do
      i >= tuple_size(vals) -> {0, best}
      !elem(ok, i) -> {0, best}
      true ->
        {left, best} = dfs(2 * i + 1, vals, ok, best)
        {right, best} = dfs(2 * i + 2, vals, ok, best)
        left = max(left, 0)
        right = max(right, 0)
        value = elem(vals, i)
        best = max(best, value + left + right)
        {value + max(left, right), best}
    end
  end

  def main do
    lines = IO.read(:stdio, :eof) |> String.split(~r/\r?\n/, trim: true)
    if lines != [] do
      tc = hd(lines) |> String.to_integer()
      {_, out} = Enum.reduce(1..tc, {1, []}, fn _, {idx, out} ->
        n = Enum.at(lines, idx) |> String.to_integer()
        toks = Enum.slice(lines, idx + 1, n)
        vals = toks |> Enum.map(fn tok -> if tok == "null", do: 0, else: String.to_integer(tok) end) |> List.to_tuple()
        ok = toks |> Enum.map(&(&1 != "null")) |> List.to_tuple()
        {idx + n + 1, out ++ [Integer.to_string(solve(vals, ok))]}
      end)
      IO.write(Enum.join(out, "\n"))
    end
  end
end

Main.main()
