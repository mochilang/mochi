defmodule Main do
  def solve(s, t) do
    chars_s = String.to_charlist(s)
    chars_t = String.to_charlist(t)
    base = List.duplicate(0, length(chars_t) + 1) |> List.replace_at(0, 1)

    Enum.reduce(chars_s, base, fn ch, dp ->
      Enum.reduce((length(chars_t) - 1)..0, dp, fn j, cur ->
        if Enum.at(chars_t, j) == ch do
          List.replace_at(cur, j + 1, Enum.at(cur, j + 1) + Enum.at(cur, j))
        else
          cur
        end
      end)
    end)
    |> List.last()
  end

  def main do
    lines = IO.read(:stdio, :eof) |> String.split(~r/\r?\n/, trim: true)
    if lines != [] do
      tc = hd(lines) |> String.to_integer()
      out = Enum.map(0..(tc - 1), fn i -> Integer.to_string(solve(Enum.at(lines, 1 + 2 * i), Enum.at(lines, 2 + 2 * i))) end)
      IO.write(Enum.join(out, "\n"))
    end
  end
end

Main.main()
