def boom
  puts 'boom'
  true
end

puts ((1 < 2) && (2 < 3) && (3 < 4)).to_s
puts ((1 < 2) && (2 > 3) && boom).to_s
puts ((1 < 2) && (2 < 3) && (3 > 4) && boom).to_s
