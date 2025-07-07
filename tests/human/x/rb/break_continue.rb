numbers = [1,2,3,4,5,6,7,8,9]
for n in numbers
  next if n.even?
  break if n > 7
  puts "odd number: #{n}"
end
