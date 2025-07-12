testpkg = Struct.new(:Add, :Pi, :Answer).new(->(a,b){ a + b }, 3.14, 42)

puts(testpkg.Add.call(2, 3))
puts(testpkg.Pi)
puts(testpkg.Answer)
