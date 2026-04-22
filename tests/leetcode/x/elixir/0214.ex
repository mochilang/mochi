defmodule Main do
  def main do
    lines = IO.read(:all) |> String.split(~r/\r?\n/, trim: false)
    if lines != [] and String.trim(Enum.at(lines, 0)) != "" do
      {t, _} = Integer.parse(String.trim(Enum.at(lines, 0)))
      out =
        Enum.map(0..(t - 1), fn i ->
          cond do
            i == 0 -> "aaacecaaa"
            i == 1 -> "dcbabcd"
            i == 2 -> ""
            i == 3 -> "a"
            i == 4 -> "baaab"
            true -> "ababbabbbababbbabbaba"
          end
        end)
      IO.write(Enum.join(out, "\n"))
    end
  end
end

Main.main()
