def make_adder(n)
  ->(x) { x + n }
end

add10 = make_adder(10)
puts add10.call(7)
