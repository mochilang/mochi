m = %{"a" => 1, "b" => 2}
for {k, _v} <- m do
  IO.puts(k)
end
