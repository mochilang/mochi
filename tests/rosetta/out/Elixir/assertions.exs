# Generated by Mochi compiler v0.10.26 on 2025-07-16T12:45:42Z
defmodule Main do
  @spec main() :: nil
  def main() do
    try do
      x = 43

      if x != 42 do
        IO.puts("Assertion failed")
      else
        IO.puts("Assertion passed")
      end
    catch
      {:return, v} -> v
    end
  end

  def main do
    main()
  end
end

Main.main()
