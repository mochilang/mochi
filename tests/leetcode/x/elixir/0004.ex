defmodule Main do
  def median(a, b) do
    m = Enum.sort(a ++ b)
    n = length(m)
    if rem(n, 2) == 1, do: Enum.at(m, div(n, 2)) * 1.0, else: (Enum.at(m, div(n, 2) - 1) + Enum.at(m, div(n, 2))) / 2.0
  end

  def main do
    lines = IO.read(:stdio, :eof) |> String.split(~r/\r?\n/, trim: true)
    if lines != [] do
      t = hd(lines) |> String.to_integer()
      {_, out} = Enum.reduce(1..t, {1, []}, fn _, {idx, out} ->
        n = Enum.at(lines, idx) |> String.to_integer()
        a =
          if n == 0 do
            []
          else
            Enum.map((idx + 1)..(idx + n), fn j -> Enum.at(lines, j) |> String.to_integer() end)
          end
        idx = idx + n + 1
        m = Enum.at(lines, idx) |> String.to_integer()
        b =
          if m == 0 do
            []
          else
            Enum.map((idx + 1)..(idx + m), fn j -> Enum.at(lines, j) |> String.to_integer() end)
          end
        {idx + m + 1, out ++ [:erlang.float_to_binary(median(a,b), decimals: 1)]}
      end)
      IO.write(Enum.join(out, "\n"))
    end
  end
end

Main.main()
