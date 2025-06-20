def longestCommonPrefix(strs)
	if ((strs).length == 0)
		return ""
	end
	prefix = strs[0]
	for i in 1...(strs).length
		j = 0
		current = strs[i]
		while ((j < (prefix).length) && (j < (current).length))
			if (prefix[j] != current[j])
				break
			end
			j = (j + 1)
		end
		prefix = prefix[0...j]
		if (prefix == "")
			break
		end
	end
	return prefix
end

