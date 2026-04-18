defmodule Main do
  @pairs %{?) => ?(, ?] => ?[, ?} => ?{}

  def valid?(s), do: valid_chars?(String.to_charlist(s), [])

  defp valid_chars?([], stack), do: stack == []
  defp valid_chars?([ch | rest], stack) when ch in ~c"([{" do
    valid_chars?(rest, [ch | stack])
  end
  defp valid_chars?([ch | rest], [open | stack]) do
    if @pairs[ch] == open, do: valid_chars?(rest, stack), else: false
  end
  defp valid_chars?([_ | _], []), do: false

  def run do
    tokens = IO.read(:eof) |> String.split(~r/\s+/, trim: true)
    if tokens != [] do
      [t | rest] = tokens
      rest
      |> Enum.take(String.to_integer(t))
      |> Enum.each(fn s -> IO.puts(if valid?(s), do: "true", else: "false") end)
    end
  end
end

Main.run()
