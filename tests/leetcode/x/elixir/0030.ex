defmodule Main do
  def solve_case(s, words) do
    if words == [] do
      []
    else
      wlen = String.length(hd(words))
      total = wlen * length(words)
      target = Enum.sort(words)
      if String.length(s) < total do
        []
      else
        Enum.reduce(0..(String.length(s) - total), [], fn i, acc ->
          parts = Enum.map(0..(length(words) - 1), fn j -> String.slice(s, i + j * wlen, wlen) end) |> Enum.sort()
          if parts == target, do: acc ++ [i], else: acc
        end)
      end
    end
  end
  def main do
    lines = IO.read(:stdio, :eof) |> String.split(~r/\r?\n/, trim: true)
    if lines != [] do
      t = hd(lines) |> String.to_integer()
      {_, out} = Enum.reduce(1..t, {1, []}, fn _, {idx, out} ->
        s = Enum.at(lines, idx)
        m = Enum.at(lines, idx + 1) |> String.to_integer()
        words = Enum.map((idx + 2)..(idx + 1 + m), fn j -> Enum.at(lines, j) end)
        {idx + 2 + m, out ++ ["[" <> Enum.join(Enum.map(solve_case(s, words), &Integer.to_string/1), ",") <> "]"]}
      end)
      IO.write(Enum.join(out, "\n"))
    end
  end
end
Main.main()
