outer = fn x ->
  inner = fn y -> x + y end
  inner.(5)
end
IO.inspect(outer.(3))
