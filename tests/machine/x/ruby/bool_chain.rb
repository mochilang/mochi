def boom()
	puts(["boom"].join(" "))
	return true
end

puts([((((1 < 2)) && ((2 < 3))) && ((3 < 4)))].join(" "))
puts([((((1 < 2)) && ((2 > 3))) && boom())].join(" "))
puts([(((((1 < 2)) && ((2 < 3))) && ((3 > 4))) && boom())].join(" "))
