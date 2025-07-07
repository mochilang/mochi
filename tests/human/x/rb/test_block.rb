# No built-in test framework; simulate
x = 1 + 2
raise "test failed" unless x == 3
puts "ok"
