def lengthOfLongestSubstring(s)
	n = (s).length
	start = 0
	best = 0
	i = 0
	while (i < n)
		j = start
		while (j < i)
			if (s[j] == s[i])
				start = (j + 1)
				break
			end
			j = (j + 1)
		end
		length = ((i - start) + 1)
		if (length > best)
			best = length
		end
		i = (i + 1)
	end
	return best
end

