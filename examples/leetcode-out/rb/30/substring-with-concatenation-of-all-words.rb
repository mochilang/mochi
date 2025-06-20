def findSubstring(s, words)
	if ((words).length == 0)
		return []
	end
	wordLen = (words[0]).length
	wordCount = (words).length
	totalLen = (wordLen * wordCount)
	if ((s).length < totalLen)
		return []
	end
	freq = {}
	for w in words
		if (freq.include?(w))
			freq[w] = (freq[w] + 1)
		else
			freq[w] = 1
		end
	end
	result = []
	for offset in 0...wordLen
		left = offset
		count = 0
		seen = {}
		j = offset
		while ((j + wordLen) <= (s).length)
			word = s[j...(j + wordLen)]
			j = (j + wordLen)
			if (freq.include?(word))
				if (seen.include?(word))
					seen[word] = (seen[word] + 1)
				else
					seen[word] = 1
				end
				count = (count + 1)
				while (seen[word] > freq[word])
					lw = s[left...(left + wordLen)]
					seen[lw] = (seen[lw] - 1)
					left = (left + wordLen)
					count = (count - 1)
				end
				if (count == wordCount)
					result = (result + [left])
					lw = s[left...(left + wordLen)]
					seen[lw] = (seen[lw] - 1)
					left = (left + wordLen)
					count = (count - 1)
				end
			else
				seen = {}
				count = 0
				left = j
			end
		end
	end
	return result
end

