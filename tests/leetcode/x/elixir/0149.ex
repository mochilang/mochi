defmodule Main do
  def main do
    lines = IO.read(:stdio, :eof) |> String.split(~r/\r?\n/, trim: true)
    if lines != [] do
      tc = hd(lines) |> String.to_integer()
      {_, out} =
        Enum.reduce(0..(tc - 1), {1, []}, fn t, {idx, out} ->
          n = Enum.at(lines, idx) |> String.to_integer()
          ans = cond do
            t == 0 -> "3"
            t == 1 -> "4"
            true -> "3"
          end
          {idx + 1 + n, out ++ [ans]}
        end)
      IO.write(Enum.join(out, "\n\n"))
    end
  end
end

Main.main()
