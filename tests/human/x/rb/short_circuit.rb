def boom(a, b)
  puts 'boom'
  true
end

puts false && boom(1, 2)
puts true || boom(1, 2)
