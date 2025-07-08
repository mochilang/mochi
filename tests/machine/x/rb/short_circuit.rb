def boom(a, b)
	puts(["boom"].join(" "))
	return true
end

puts([(false && boom(1, 2))].join(" "))
puts([(true || boom(1, 2))].join(" "))
