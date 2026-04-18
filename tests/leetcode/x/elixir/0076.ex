defmodule Main do
  def min_window(s, t) do
    need = Enum.reduce(String.to_charlist(t), %{}, fn ch, acc -> Map.update(acc, ch, 1, &(&1 + 1)) end)
    chars = String.to_charlist(s)
    scan(chars, need, String.length(t), 0, 0, {length(chars) + 1, 0}, chars)
  end
  defp scan(chars, _need, _missing, right, _left, {best_len, best_start}, _orig) when right == length(chars), do: if(best_len > length(chars), do: "", else: chars |> Enum.slice(best_start, best_len) |> to_string())
  defp scan(chars, need, missing, right, left, best, orig) do
    ch = Enum.at(chars, right)
    cur = Map.get(need, ch, 0)
    missing = if cur > 0, do: missing - 1, else: missing
    need = Map.put(need, ch, cur - 1)
    shrink(chars, need, missing, right + 1, left, best, orig)
  end
  defp shrink(chars, need, missing, right, left, {best_len, best_start}, orig) do
    if missing != 0 do
      scan(chars, need, missing, right, left, {best_len, best_start}, orig)
    else
      {best_len, best_start} = if right - left < best_len, do: {right - left, left}, else: {best_len, best_start}
      ch = Enum.at(chars, left)
      cur = Map.get(need, ch, 0)
      need = Map.put(need, ch, cur + 1)
      missing = if cur + 1 > 0, do: missing + 1, else: missing
      shrink(chars, need, missing, right, left + 1, {best_len, best_start}, orig)
    end
  end
  def main do
    lines = IO.read(:eof) |> String.split("\n", trim: true)
    case lines do
      [] -> :ok
      [tstr | rest] ->
        {t, _} = Integer.parse(tstr)
        out = for i <- 0..(t - 1), do: min_window(Enum.at(rest, 2*i), Enum.at(rest, 2*i + 1))
        IO.write(Enum.join(out, "\n"))
    end
  end
end
Main.main()
