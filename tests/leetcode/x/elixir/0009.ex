defmodule Main do
  def is_palindrome(x) when x < 0, do: false
  def is_palindrome(x), do: x == reverse_digits(x, 0)

  defp reverse_digits(0, rev), do: rev
  defp reverse_digits(x, rev), do: reverse_digits(div(x, 10), rev * 10 + rem(x, 10))

  def run do
    tokens = IO.read(:all) |> String.split(~r/\s+/, trim: true)
    if tokens != [] do
      [t | rest] = Enum.map(tokens, &String.to_integer/1)
      rest |> Enum.take(t) |> Enum.each(fn x -> IO.puts(if is_palindrome(x), do: "true", else: "false") end)
    end
  end
end

Main.run()
