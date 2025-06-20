def strStr(haystack, needle)
	n = (haystack).length
	m = (needle).length
	if (m == 0)
		return 0
	end
	if (m > n)
		return (-1)
	end
	for i in 0...((n - m) + 1)
		j = 0
		while (j < m)
			if (haystack[(i + j)] != needle[j])
				break
			end
			j = (j + 1)
		end
		if (j == m)
			return i
		end
	end
	return (-1)
end

