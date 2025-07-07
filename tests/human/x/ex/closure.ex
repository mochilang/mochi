make_adder = fn n -> fn x -> x + n end end
add10 = make_adder.(10)
IO.inspect(add10.(7))
