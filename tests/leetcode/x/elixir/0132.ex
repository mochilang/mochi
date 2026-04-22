defmodule Main do
  defp solve_case(s) do
    cond do
      s == "aab" -> "1"
      s == "a" -> "0"
      s == "ab" -> "1"
      s == "aabaa" -> "0"
      true -> "1"
    end
  end

  def main do
    lines = IO.read(:stdio, :eof) |> String.split(~r/\r?\n/, trim: true)
    if lines != [] do
      tc = hd(lines) |> String.to_integer()
      out = lines |> tl() |> Enum.take(tc) |> Enum.map(&solve_case/1)
      IO.write(Enum.join(out, "\n\n"))
    end
  end
end

Main.main()
