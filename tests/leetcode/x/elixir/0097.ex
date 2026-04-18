defmodule Main do
  def solve(s1, s2, s3) do
    m = String.length(s1)
    n = String.length(s2)
    if m + n != String.length(s3) do
      false
    else
      dp =
        Enum.reduce(0..m, %{{0, 0} => true}, fn i, acc ->
          Enum.reduce(0..n, acc, fn j, acc2 ->
            v1 = i > 0 and Map.get(acc2, {i - 1, j}, false) and String.at(s1, i - 1) == String.at(s3, i + j - 1)
            v2 = j > 0 and Map.get(acc2, {i, j - 1}, false) and String.at(s2, j - 1) == String.at(s3, i + j - 1)
            if v1 or v2 or (i == 0 and j == 0), do: Map.put(acc2, {i, j}, true), else: acc2
          end)
        end)
      Map.get(dp, {m, n}, false)
    end
  end

  def main do
    lines = IO.read(:all) |> String.split(~r/\r?\n/, trim: false)
    if lines != [] and String.trim(Enum.at(lines, 0)) != "" do
      {t, _} = Integer.parse(String.trim(Enum.at(lines, 0)))
      out = Enum.map(0..(t - 1), fn i -> if solve(Enum.at(lines, 1 + 3 * i), Enum.at(lines, 2 + 3 * i), Enum.at(lines, 3 + 3 * i)), do: "true", else: "false" end)
      IO.write(Enum.join(out, "\n"))
    end
  end
end

Main.main()
