def intSqrt(x)
	if (x < 2)
		return x
	end
	left = 1
	right = (x / 2)
	ans = 0
	while (left <= right)
		mid = (left + (((right - left)) / 2))
		sq = (mid * mid)
		if (sq == x)
			return mid
		end
		if (sq < x)
			left = (mid + 1)
			ans = mid
		else
			right = (mid - 1)
		end
	end
	return ans
end

def sumRecip(n)
	s = 1
	limit = intSqrt(n)
	f = 2
	while (f <= limit)
		if ((n % f) == 0)
			s = (s + (n / f))
			f2 = (n / f)
			if (f2 != f)
				s = (s + f)
			end
		end
		f = (f + 1)
	end
	return s
end

def main()
	nums = [6, 28, 120, 496, 672, 8128, 30240, 32760, 523776]
	nums.each do |n|
		s = sumRecip(n)
		if ((s % n) == 0)
			val = (s / n)
			perfect = ""
			if (val == 1)
				perfect = "perfect!"
			end
			puts(((((("Sum of recipr. factors of " + (n).to_s) + " = ") + (val).to_s) + " exactly ") + perfect))
		end
	end
end

main()
