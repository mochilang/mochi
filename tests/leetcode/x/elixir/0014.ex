defmodule Main do
  def lcp([first | rest]), do: reduce(first, [first | rest])

  defp reduce(prefix, strs) do
    if Enum.all?(strs, &String.starts_with?(&1, prefix)) do
      prefix
    else
      reduce(String.slice(prefix, 0, String.length(prefix) - 1), strs)
    end
  end

  def run do
    tokens = IO.read(:all) |> String.split(~r/\s+/, trim: true)
    if tokens != [] do
      [t | rest] = tokens
      solve(rest, String.to_integer(t))
    end
  end

  defp solve(_rest, 0), do: :ok
  defp solve([n | rest], t) do
    count = String.to_integer(n)
    {strs, tail} = Enum.split(rest, count)
    IO.puts("\"#{lcp(strs)}\"")
    solve(tail, t - 1)
  end
end

Main.run()
