# Generated by Mochi Elixir compiler
defmodule Main do
  def isPalindrome(x) do
    try do
      if x < 0 do
        throw({:return, false})
      end

      s = to_string(x)
      n = length(s)

      for i <- 0..(n / 2 - 1) do
        if Enum.at(s, i) != Enum.at(s, n - 1 - i) do
          throw({:return, false})
        end
      end

      throw({:return, true})
    catch
      {:return, v} -> v
    end
  end

  def main do
  end
end

Main.main()
