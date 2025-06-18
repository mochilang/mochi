def isPalindrome(x)
	if (x < 0)
		return false
	end
	s = (x).to_s
	n = (s).length
	for i in 0...(n / 2)
		if (s[i] != s[((n - 1) - i)])
			return false
		end
	end
	return true
end

