defmodule Main do
  def solve_case(s) do
    chars = String.graphemes(s)
    {_, best} = Enum.reduce(Enum.with_index(chars), {[-1], 0}, fn {ch, i}, {stack, best} ->
      if ch == "(" do
        {[i | stack], best}
      else
        stack = tl(stack)
        if stack == [] do
          {[i], best}
        else
          {stack, max(best, i - hd(stack))}
        end
      end
    end)
    best
  end
  def main do
    lines = IO.read(:stdio, :eof) |> String.split(~r/\r?\n/, trim: false)
    if lines != [] and hd(lines) != "" do
      t = hd(lines) |> String.to_integer()
      {_, out} = Enum.reduce(1..t, {1, []}, fn _, {idx, out} ->
        n = Enum.at(lines, idx) |> String.to_integer()
        s = if n > 0, do: Enum.at(lines, idx + 1), else: ""
        {idx + 1 + if(n > 0, do: 1, else: 0), out ++ [Integer.to_string(solve_case(s))]}
      end)
      IO.write(Enum.join(out, "\n"))
    end
  end
end
Main.main()
