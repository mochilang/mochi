# https://www.spoj.com/problems/TEST/
defmodule Main do
  def loop do
    case IO.gets("") do
      nil -> :ok
      line ->
        n = String.to_integer(String.trim(line))
        if n == 42 do
          :ok
        else
          IO.puts(n)
          loop()
        end
    end
  end
end

Main.loop()
