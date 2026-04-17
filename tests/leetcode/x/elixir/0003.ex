defmodule Main do
  def longest(s) do
    chars = String.graphemes(s)
    {_last, _left, best} = Enum.with_index(chars) |> Enum.reduce({%{}, 0, 0}, fn {ch, right}, {last, left, best} ->
      left2 = if Map.has_key?(last, ch) and Map.get(last, ch) >= left, do: Map.get(last, ch) + 1, else: left
      best2 = max(best, right - left2 + 1)
      {Map.put(last, ch, right), left2, best2}
    end)
    best
  end
  def run do
    lines = IO.read(:eof) |> String.split(~r/?
/, trim: false)
    case lines do
      [] -> :ok
      [first | rest] ->
        if String.trim(first) != "" do
          {t, _} = Integer.parse(String.trim(first))
          out = for i <- 0..(t - 1), do: Integer.to_string(longest(Enum.at(rest, i, "")))
          IO.write(Enum.join(out, "\n"))
        end
    end
  end
end
Main.run()
