# Generated by Mochi compiler v0.10.28 on 2025-07-18T07:03:29Z
defmodule Main do
  @spec makeAdder(integer()) :: (integer() -> integer())
  def makeAdder(n) do
    try do
      throw({:return, fn x -> x + n end})
    catch
      {:return, v} -> v
    end
  end

  def main do
    # add10 :: (integer() -> integer())
    add10 = makeAdder(10)
    IO.inspect(add10.(7))
  end
end

Main.main()
