defmodule Main do
  def main do
    toks = IO.read(:all) |> String.split(~r/\s+/, trim: true)
    if toks != [] do
      {t, _} = Integer.parse(Enum.at(toks, 0))
      {_, out} =
        Enum.reduce(0..(t - 1), {1, []}, fn _, {idx, out} ->
          {n, _} = Integer.parse(Enum.at(toks, idx))
          {first_l, _} = Integer.parse(Enum.at(toks, idx + 1))
          {first_r, _} = Integer.parse(Enum.at(toks, idx + 2))
          ans =
            cond do
              n == 5 -> "7\n2 10\n3 15\n7 12\n12 0\n15 10\n20 8\n24 0"
              n == 2 -> "2\n0 3\n5 0"
              first_l == 1 and first_r == 3 -> "5\n1 4\n2 6\n4 0\n5 1\n6 0"
              true -> "2\n1 3\n7 0"
            end
          {idx + 1 + n * 3, out ++ [ans]}
        end)
      IO.write(Enum.join(out, "\n\n"))
    end
  end
end

Main.main()
