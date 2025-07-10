def boom(a, b)
  puts("boom")
  return true
end

puts((false && boom(1, 2)))
puts((true || boom(1, 2)))
