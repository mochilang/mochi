defmodule Main do
  @int_min -2147483648
  @int_max 2147483647

  def reverse_int(x), do: reverse_int(x, 0)
  defp reverse_int(0, ans), do: ans
  defp reverse_int(x, ans) do
    digit = rem(x, 10)
    x = div(x, 10)
    cond do
      ans > div(@int_max, 10) or (ans == div(@int_max, 10) and digit > 7) -> 0
      ans < div(@int_min, 10) or (ans == div(@int_min, 10) and digit < -8) -> 0
      true -> reverse_int(x, ans * 10 + digit)
    end
  end

  def main do
    lines = IO.read(:stdio, :eof) |> String.split(~r/\r?\n/, trim: true)
    if lines != [] do
      t = hd(lines) |> String.to_integer()
      out =
        lines
        |> tl()
        |> Enum.take(t)
        |> Enum.map(fn line -> line |> String.to_integer() |> reverse_int() |> Integer.to_string() end)
      IO.write(Enum.join(out, "\n"))
    end
  end
end

Main.main()
