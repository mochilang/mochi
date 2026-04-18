defmodule Main do
  def max_area(h) do
    do_max(h, 0, length(h) - 1, 0)
  end

  defp do_max(_, l, r, best) when l >= r, do: best
  defp do_max(h, l, r, best) do
    left = Enum.at(h, l)
    right = Enum.at(h, r)
    height = min(left, right)
    best = max(best, (r - l) * height)
    if left < right, do: do_max(h, l + 1, r, best), else: do_max(h, l, r - 1, best)
  end

  def main do
    lines = IO.read(:stdio, :eof) |> String.split(~r/\r?\n/, trim: true)
    if lines != [] do
      t = hd(lines) |> String.to_integer()
      {_, out} = Enum.reduce(1..t, {1, []}, fn _, {idx, out} ->
        n = Enum.at(lines, idx) |> String.to_integer()
        vals = Enum.map((idx + 1)..(idx + n), fn j -> Enum.at(lines, j) |> String.to_integer() end)
        {idx + n + 1, out ++ [Integer.to_string(max_area(vals))]}
      end)
      IO.write(Enum.join(out, "\n"))
    end
  end
end

Main.main()
