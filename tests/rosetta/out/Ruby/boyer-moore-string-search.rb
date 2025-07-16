def _sliceString(s, i, j)
  start = i
  finish = j
  chars = s.chars
  n = chars.length
  start += n if start < 0
  finish += n if finish < 0
  start = 0 if start < 0
  finish = n if finish > n
  finish = start if finish < start
  chars[start...finish].join
end

def indexOfStr(h, n)
	hlen = (h).length
	nlen = (n).length
	if (nlen == 0)
		return 0
	end
	i = 0
	while (i <= (hlen - nlen))
		if (_sliceString(h, i, (i + nlen)) == n)
			return i
		end
		i = (i + 1)
	end
	return (-1)
end

def stringSearchSingle(h, n)
	return indexOfStr(h, n)
end

def stringSearch(h, n)
	result = []
	start = 0
	hlen = (h).length
	nlen = (n).length
	while (start < hlen)
		idx = indexOfStr(_sliceString(h, start, hlen), n)
		if (idx >= 0)
			result = (result + [(start + idx)])
			start = ((start + idx) + nlen)
		else
			break
		end
	end
	return result
end

def display(nums)
	s = "["
	i = 0
	while (i < (nums).length)
		if (i > 0)
			s = (s + ", ")
		end
		s = (s + (nums[i]).to_s)
		i = (i + 1)
	end
	s = (s + "]")
	return s
end

def main()
	texts = ["GCTAGCTCTACGAGTCTA", "GGCTATAATGCGTA", "there would have been a time for such a word", "needle need noodle needle", "DKnuthusesandprogramsanimaginarycomputertheMIXanditsassociatedmachinecodeandassemblylanguages", "Nearby farms grew an acre of alfalfa on the dairy's behalf, with bales of that alfalfa exchanged for milk."]
	patterns = ["TCTA", "TAATAAA", "word", "needle", "and", "alfalfa"]
	i = 0
	while (i < (texts).length)
		puts(((("text" + ((i + 1)).to_s) + " = ") + texts[i]))
		i = (i + 1)
	end
	puts("")
	j = 0
	while (j < (texts).length)
		idxs = stringSearch(texts[j], patterns[j])
		puts(((((("Found \"" + patterns[j]) + "\" in 'text") + ((j + 1)).to_s) + "' at indexes ") + display(idxs)))
		j = (j + 1)
	end
end

main()
