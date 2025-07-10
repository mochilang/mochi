$numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9]
$numbers.each do |n|
	if ((n % 2) == 0)
		next
	end
	if (n > 7)
		break
	end
	puts(["odd number:", n].join(" "))
end
