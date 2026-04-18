defmodule Main do
  @values %{"I" => 1, "V" => 5, "X" => 10, "L" => 50, "C" => 100, "D" => 500, "M" => 1000}

  def roman_to_int(s) do
    chars = String.graphemes(s)
    Enum.with_index(chars)
    |> Enum.reduce(0, fn {ch, i}, acc ->
      cur = @values[ch]
      nxt = if i + 1 < length(chars), do: @values[Enum.at(chars, i + 1)], else: 0
      acc + if cur < nxt, do: -cur, else: cur
    end)
  end

  def run do
    tokens = IO.read(:all) |> String.split(~r/\s+/, trim: true)
    if tokens != [] do
      [t | rest] = tokens
      rest |> Enum.take(String.to_integer(t)) |> Enum.each(fn s -> IO.puts(roman_to_int(s)) end)
    end
  end
end

Main.run()
