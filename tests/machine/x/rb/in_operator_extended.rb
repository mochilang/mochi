require 'ostruct'

xs = [1, 2, 3]
ys = (((xs)).select { |x| ((x % 2) == 1) }).map { |x| x }
puts((ys.include?(1)))
puts((ys.include?(2)))
m = OpenStruct.new(a: 1)
puts((m.to_h.key?("a")))
puts((m.to_h.key?("b")))
s = "hello"
puts((s.include?("ell")))
puts((s.include?("foo")))
