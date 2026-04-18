defmodule Main do
  def my_atoi(s) do
    chars = String.to_charlist(s)
    chars = Enum.drop_while(chars, &(&1 == ? ))
    {sign, chars} = case chars do
      [?- | rest] -> {-1, rest}
      [?+ | rest] -> {1, rest}
      _ -> {1, chars}
    end
    limit = if sign > 0, do: 7, else: 8
    parse(chars, sign, limit, 0)
  end

  defp parse([ch | rest], sign, limit, ans) when ch >= ?0 and ch <= ?9 do
    digit = ch - ?0
    if ans > 214748364 or (ans == 214748364 and digit > limit) do
      if sign > 0, do: 2147483647, else: -2147483648
    else
      parse(rest, sign, limit, ans * 10 + digit)
    end
  end
  defp parse(_, sign, _, ans), do: sign * ans

  def main do
    lines = IO.read(:stdio, :eof) |> String.split(~r/\r?\n/, trim: false)
    if lines != [] do
      t = hd(lines) |> String.trim() |> String.to_integer()
      out = Enum.map(0..(t - 1), fn i -> Integer.to_string(my_atoi(Enum.at(lines, i + 1, ""))) end)
      IO.write(Enum.join(out, "\n"))
    end
  end
end

Main.main()
