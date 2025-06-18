def expand(s, left, right)
	l = left
	r = right
	n = (s).length
	while ((l >= 0) && (r < n))
		if (s[l] != s[r])
			break
		end
		l = (l - 1)
		r = (r + 1)
	end
	return ((r - l) - 1)
end

def longestPalindrome(s)
	if ((s).length <= 1)
		return s
	end
	start = 0
	_end = 0
	n = (s).length
	for i in 0...n
		len1 = expand(s, i, i)
		len2 = expand(s, i, (i + 1))
		l = len1
		if (len2 > len1)
			l = len2
		end
		if (l > (_end - start))
			start = (i - (((l - 1)) / 2))
			_end = (i + (l / 2))
		end
	end
	return s[start...(_end + 1)]
end

