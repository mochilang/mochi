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

def indexOf(s, ch)
	i = 0
	while (i < (s).length)
		if (s[i...(i + 1)] == ch)
			return i
		end
		i = (i + 1)
	end
	return (-1)
end

def rotate(s, n)
	return (s[n..-1] + s[0...n])
end

def scrambleLeft(s)
	return (((s[0...1] + s[2...14]) + s[1...2]) + s[14..-1])
end

def scrambleRight(s)
	return ((((s[1...3] + s[4...15]) + s[3...4]) + s[15..-1]) + s[0...1])
end

def chao(text, encode)
	left = "HXUCZVAMDSLKPEFJRIGTWOBNYQ"
	right = "PTLNBQDEOYSFAVZKGJRIHWXUMC"
	out = ""
	i = 0
	while (i < (text).length)
		ch = text[i...(i + 1)]
		idx = 0
		if encode
			idx = indexOf(right, ch)
			out = (out + _sliceString(left, idx, (idx + 1)))
		else
			idx = indexOf(left, ch)
			out = (out + _sliceString(right, idx, (idx + 1)))
		end
		left = rotate(left, idx)
		right = rotate(right, idx)
		left = scrambleLeft(left)
		right = scrambleRight(right)
		i = (i + 1)
	end
	return out
end

def main()
	plain = "WELLDONEISBETTERTHANWELLSAID"
	cipher = chao(plain, true)
	puts(plain)
	puts(cipher)
	puts(chao(cipher, false))
end

main()
