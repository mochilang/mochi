# Generated by Mochi compiler v0.10.28 on 2025-07-18T07:04:20Z
defmodule Main do
  def main do
    # items :: list(map())
    items = [%{n: 1, v: "a"}, %{n: 1, v: "b"}, %{n: 2, v: "c"}]
    # result :: list(any())
    result = for i <- Enum.sort_by(items, fn i -> i.n end), do: i.v
    IO.inspect(result)
  end
end

Main.main()
