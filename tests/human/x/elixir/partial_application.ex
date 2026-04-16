add = fn a, b -> a + b end
add5 = fn b -> add.(5, b) end
IO.inspect(add5.(3))
