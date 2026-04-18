defmodule Main do
  def reverse_groups(arr, k) do
    do_reverse(arr, k, [])
  end

  defp do_reverse(arr, k, acc) do
    if length(arr) < k do
      acc ++ arr
    else
      chunk = Enum.take(arr, k) |> Enum.reverse()
      rest = Enum.drop(arr, k)
      do_reverse(rest, k, acc ++ chunk)
    end
  end

  def solve(lines, idx, t, out) do
    if t == 0 do
      Enum.join(Enum.reverse(out), "\n")
    else
      n = Enum.at(lines, idx) |> String.to_integer()
      arr = Enum.map((idx + 1)..(idx + n), fn j -> Enum.at(lines, j) |> String.to_integer() end)
      k = Enum.at(lines, idx + n + 1) |> String.to_integer()
      ans = "[" <> Enum.join(Enum.map(reverse_groups(arr, k), &Integer.to_string/1), ",") <> "]"
      solve(lines, idx + n + 2, t - 1, [ans | out])
    end
  end

  def main do
    lines = IO.read(:stdio, :eof) |> String.split(~r/\r?\n/, trim: true)
    if lines != [] do
      t = hd(lines) |> String.to_integer()
      IO.write(solve(lines, 1, t, []))
    end
  end
end

Main.main()
