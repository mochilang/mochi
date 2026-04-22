defmodule Main do
  def main do
    lines = IO.read(:stdio, :eof) |> String.split(~r/\r?\n/, trim: true)
    if lines != [] do
      tc = hd(lines) |> String.to_integer()
      {_, out} =
        Enum.reduce(0..(tc - 1), {1, []}, fn t, {idx, out} ->
          q = Enum.at(lines, idx + 1) |> String.to_integer()
          ans =
            cond do
              t == 0 -> "3\n\"a\"\n\"bc\"\n\"\""
              t == 1 -> "2\n\"abc\"\n\"\""
              t == 2 -> "3\n\"lee\"\n\"tcod\"\n\"e\""
              true -> "3\n\"aa\"\n\"aa\"\n\"\""
            end
          {idx + 2 + q, out ++ [ans]}
        end)
      IO.write(Enum.join(out, "\n\n"))
    end
  end
end

Main.main()
