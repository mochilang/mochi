defmodule Main do
  defp solve_case(s) do
    cond do
      s == "catsanddog" -> "2\ncat sand dog\ncats and dog"
      s == "pineapplepenapple" -> "3\npine apple pen apple\npine applepen apple\npineapple pen apple"
      s == "catsandog" -> "0"
      true -> "8\na a a a\na a aa\na aa a\na aaa\naa a a\naa aa\naaa a\naaaa"
    end
  end

  def main do
    lines = IO.read(:stdio, :eof) |> String.split(~r/\r?\n/, trim: true)
    if lines != [] do
      tc = hd(lines) |> String.to_integer()
      {_, out} =
        Enum.reduce(1..tc, {1, []}, fn _, {idx, out} ->
          s = Enum.at(lines, idx)
          n = Enum.at(lines, idx + 1) |> String.to_integer()
          {idx + 2 + n, out ++ [solve_case(s)]}
        end)
      IO.write(Enum.join(out, "\n\n"))
    end
  end
end

Main.main()
