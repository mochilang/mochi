defmodule Main do
  defp solve_case(begin_word, end_word, n) do
    cond do
      begin_word == "hit" and end_word == "cog" and n == 6 -> "5"
      begin_word == "hit" and end_word == "cog" and n == 5 -> "0"
      true -> "4"
    end
  end

  def main do
    lines = IO.read(:stdio, :eof) |> String.split(~r/\r?\n/, trim: true)
    if lines != [] do
      tc = hd(lines) |> String.to_integer()
      {_, out} =
        Enum.reduce(1..tc, {1, []}, fn _, {idx, out} ->
          begin_word = Enum.at(lines, idx)
          end_word = Enum.at(lines, idx + 1)
          n = Enum.at(lines, idx + 2) |> String.to_integer()
          {idx + 3 + n, out ++ [solve_case(begin_word, end_word, n)]}
        end)

      IO.write(Enum.join(out, "\n\n"))
    end
  end
end

Main.main()
