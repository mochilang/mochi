def sum_rec(n, acc)
	if (n == 0)
		return acc
	end
	return sum_rec((n - 1), (acc + n))
end

puts(sum_rec(10, 0))
