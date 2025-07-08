require 'ostruct'

xs = [1, 2, 3]
ys = (((xs)).select { |x| ((x % 2) == 1) }).map { |x| x }
puts([(ys.include?(1))].join(" "))
puts([(ys.include?(2))].join(" "))
m = OpenStruct.new(a: 1)
puts([(m.key?("a"))].join(" "))
puts([(m.key?("b"))].join(" "))
s = "hello"
puts([(s.include?("ell"))].join(" "))
puts([(s.include?("foo"))].join(" "))
