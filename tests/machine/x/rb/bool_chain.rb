def boom()
  puts("boom")
  return true
end

puts(((((1 < 2)) && ((2 < 3))) && ((3 < 4))))
puts(((((1 < 2)) && ((2 > 3))) && boom()))
puts((((((1 < 2)) && ((2 < 3))) && ((3 > 4))) && boom()))
