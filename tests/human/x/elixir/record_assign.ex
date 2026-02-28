defmodule Counter do
  defstruct n: 0
end

inc = fn c -> %{c | n: c.n + 1} end

c = %Counter{}
c = inc.(c)
IO.inspect(c.n)
