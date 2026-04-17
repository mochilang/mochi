defmodule Main do
  def match_at(s, p, i, j) do
    if j == String.length(p) do
      i == String.length(s)
    else
      first = i < String.length(s) and (String.at(p, j) == "." or String.at(s, i) == String.at(p, j))
      if j + 1 < String.length(p) and String.at(p, j + 1) == "*" do
        match_at(s, p, i, j + 2) or (first and match_at(s, p, i + 1, j))
      else
        first and match_at(s, p, i + 1, j + 1)
      end
    end
  end

  def main do
    lines = IO.read(:stdio, :eof) |> String.split(~r/\r?\n/, trim: true)
    if lines != [] do
      t = hd(lines) |> String.to_integer()
      {_, out} = Enum.reduce(1..t, {1, []}, fn _, {idx, out} ->
        s = Enum.at(lines, idx)
        p = Enum.at(lines, idx + 1)
        {idx + 2, out ++ [if(match_at(s, p, 0, 0), do: "true", else: "false")]}
      end)
      IO.write(Enum.join(out, "\n"))
    end
  end
end

Main.main()
