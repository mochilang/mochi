def add(a, b)
  a + b
end

add5 = ->(b) { add(5, b) }
puts add5.call(3)
