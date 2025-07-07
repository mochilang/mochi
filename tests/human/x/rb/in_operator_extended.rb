xs = [1, 2, 3]
ys = xs.select { |x| x.odd? }
puts ys.include?(1)
puts ys.include?(2)

m = { a: 1 }
puts m.key?(:a)
puts m.key?(:b)

s = "hello"
puts s.include?("ell")
puts s.include?("foo")
