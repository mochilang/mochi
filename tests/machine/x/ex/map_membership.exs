# Generated by Mochi Elixir compiler
defmodule Main do
  @m %{"a" => 1, "b" => 2}
  def main do
    IO.inspect(if is_map(@m), do: Map.has_key?(@m, "a"), else: Enum.member?(@m, "a"))
    IO.inspect(if is_map(@m), do: Map.has_key?(@m, "c"), else: Enum.member?(@m, "c"))
  end
end

Main.main()
