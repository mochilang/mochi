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

def trimSpace(s)
	start = 0
	while ((start < (s).length) && (s[start...(start + 1)] == " "))
		start = (start + 1)
	end
	_end = (s).length
	while ((_end > start) && (s[(_end - 1)..._end] == " "))
		_end = (_end - 1)
	end
	return s[start..._end]
end

def isUpper(ch)
	return ((ch >= "A") && (ch <= "Z"))
end

def padLeft(s, w)
	res = ""
	n = (w - (s).length)
	while (n > 0)
		res = (res + " ")
		n = (n - 1)
	end
	return (res + s)
end

def snakeToCamel(s)
	s = trimSpace(s)
	out = ""
	up = false
	i = 0
	while (i < (s).length)
		ch = s[i...(i + 1)]
		if ((((ch == "_") || (ch == "-")) || (ch == " ")) || (ch == "."))
			up = true
			i = (i + 1)
			next
		end
		if (i == 0)
			out = (out + (ch).to_s.downcase)
			up = false
			i = (i + 1)
			next
		end
		if up
			out = (out + (ch).to_s.upcase)
			up = false
		else
			out = (out + ch)
		end
		i = (i + 1)
	end
	return out
end

def camelToSnake(s)
	s = trimSpace(s)
	out = ""
	prevUnd = false
	i = 0
	while (i < (s).length)
		ch = s[i...(i + 1)]
		if (((ch == " ") || (ch == "-")) || (ch == "."))
			if ((!prevUnd) && ((out).length > 0))
				out = (out + "_")
				prevUnd = true
			end
			i = (i + 1)
			next
		end
		if (ch == "_")
			if ((!prevUnd) && ((out).length > 0))
				out = (out + "_")
				prevUnd = true
			end
			i = (i + 1)
			next
		end
		if isUpper(ch)
			if ((i > 0) && ((!prevUnd)))
				out = (out + "_")
			end
			out = (out + (ch).to_s.downcase)
			prevUnd = false
		else
			out = (out + (ch).to_s.downcase)
			prevUnd = false
		end
		i = (i + 1)
	end
	start = 0
	while ((start < (out).length) && (_sliceString(out, start, (start + 1)) == "_"))
		start = (start + 1)
	end
	_end = (out).length
	while ((_end > start) && (_sliceString(out, (_end - 1), _end) == "_"))
		_end = (_end - 1)
	end
	out = _sliceString(out, start, _end)
	res = ""
	j = 0
	lastUnd = false
	while (j < (out).length)
		c = _sliceString(out, j, (j + 1))
		if (c == "_")
			if (!lastUnd)
				res = (res + c)
			end
			lastUnd = true
		else
			res = (res + c)
			lastUnd = false
		end
		j = (j + 1)
	end
	return res
end

def main()
	samples = ["snakeCase", "snake_case", "snake-case", "snake case", "snake CASE", "snake.case", "variable_10_case", "variable10Case", "ɛrgo rE tHis", "hurry-up-joe!", "c://my-docs/happy_Flag-Day/12.doc", " spaces "]
	puts("=== To snake_case ===")
	samples.each do |s|
		puts(((padLeft(s, 34) + " => ") + camelToSnake(s)))
	end
	puts("")
	puts("=== To camelCase ===")
	samples.each do |s|
		puts(((padLeft(s, 34) + " => ") + snakeToCamel(s)))
	end
end

main()
