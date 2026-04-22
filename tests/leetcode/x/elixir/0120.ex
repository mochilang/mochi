defmodule Main do
  def solve(tri) do
    dp = List.last(tri)
    if length(tri) == 1 do
      hd(dp)
    else
      Enum.reduce((length(tri) - 2)..0, dp, fn i, acc ->
        row = Enum.at(tri, i)
        Enum.reduce(0..i, acc, fn j, a -> List.replace_at(a, j, Enum.at(row, j) + min(Enum.at(a, j), Enum.at(a, j + 1))) end)
      end)
      |> hd()
    end
  end

  def main do
    toks = IO.read(:all) |> String.split(~r/\s+/, trim: true)
    if toks != [] do
      {t, _} = Integer.parse(Enum.at(toks, 0))
      {_, out} =
        Enum.reduce(0..(t - 1), {1, []}, fn _, {idx, out} ->
          {rows, _} = Integer.parse(Enum.at(toks, idx))
          {tri, nextIdx} =
            Enum.reduce(1..rows, {[], idx + 1}, fn r, {tri, i2} ->
              row = Enum.map(i2..(i2 + r - 1), fn k -> {v, _} = Integer.parse(Enum.at(toks, k)); v end)
              {tri ++ [row], i2 + r}
            end)
          {nextIdx, out ++ [Integer.to_string(solve(tri))]}
        end)
      IO.write(Enum.join(out, "\n"))
    end
  end
end

Main.main()
