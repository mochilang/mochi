# Generated by Mochi Elixir compiler
defmodule Main do
  def main do
    # m :: map()
    m = %{"a" => 1, "b" => 2}
    _ = m

    for k <- Map.keys(m) do
      IO.inspect(k)
    end
  end
end

Main.main()
