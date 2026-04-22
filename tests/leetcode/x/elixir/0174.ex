defmodule Main do
  def solve(dungeon) do
    cols = length(hd(dungeon))
    inf = 1_000_000_000
    dp0 = List.duplicate(inf, cols + 1) |> List.replace_at(cols - 1, 1)

    Enum.reduce((length(dungeon) - 1)..0, dp0, fn i, dp ->
      row = Enum.at(dungeon, i)
      Enum.reduce((cols - 1)..0, dp, fn j, acc ->
        need = min(Enum.at(acc, j), Enum.at(acc, j + 1)) - Enum.at(row, j)
        List.replace_at(acc, j, if(need <= 1, do: 1, else: need))
      end)
    end)
    |> hd()
  end

  def main do
    toks = IO.read(:all) |> String.split(~r/\s+/, trim: true)
    if toks != [] do
      {t, _} = Integer.parse(Enum.at(toks, 0))
      {_, out} =
        Enum.reduce(0..(t - 1), {1, []}, fn _, {idx, out} ->
          {rows, _} = Integer.parse(Enum.at(toks, idx))
          {cols, _} = Integer.parse(Enum.at(toks, idx + 1))
          {dungeon, next_idx} =
            Enum.reduce(0..(rows - 1), {[], idx + 2}, fn _, {acc, pos} ->
              row =
                Enum.map(pos..(pos + cols - 1), fn k ->
                  {v, _} = Integer.parse(Enum.at(toks, k))
                  v
                end)

              {acc ++ [row], pos + cols}
            end)

          {next_idx, out ++ [Integer.to_string(solve(dungeon))]}
        end)

      IO.write(Enum.join(out, "\n"))
    end
  end
end

Main.main()
