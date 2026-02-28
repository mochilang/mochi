x = 2
label = case x do
  1 -> "one"
  2 -> "two"
  3 -> "three"
  _ -> "unknown"
end
IO.puts(label)
