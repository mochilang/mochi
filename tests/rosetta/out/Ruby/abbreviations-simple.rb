# Generated by Mochi compiler v0.10.30 on 2025-07-19T00:24:45Z
def _indexString(s, i)
  idx = i
  chars = s.chars
  idx += chars.length if idx < 0
  raise 'index out of range' if idx < 0 || idx >= chars.length
  chars[idx]
end
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

def fields(s)
	words = []
	cur = ""
	i = 0
	while (i < (s).length)
		ch = _sliceString(s, i, (i + 1))
		if (((ch == " ") || (ch == "\n")) || (ch == "\t"))
			if ((cur).length > 0)
				words = (words + [cur])
				cur = ""
			end
		else
			cur = (cur + ch)
		end
		i = (i + 1)
	end
	if ((cur).length > 0)
		words = (words + [cur])
	end
	return words
end

def padRight(s, width)
	out = s
	i = (s).length
	while (i < width)
		out = (out + " ")
		i = (i + 1)
	end
	return out
end

def join(xs, sep)
	res = ""
	i = 0
	while (i < (xs).length)
		if (i > 0)
			res = (res + sep)
		end
		res = (res + xs[i])
		i = (i + 1)
	end
	return res
end

def parseIntStr(str)
	i = 0
	neg = false
	if (((str).length > 0) && (str[0...1] == "-"))
		neg = true
		i = 1
	end
	n = 0
	digits = {"0" => 0, "1" => 1, "2" => 2, "3" => 3, "4" => 4, "5" => 5, "6" => 6, "7" => 7, "8" => 8, "9" => 9}
	while (i < (str).length)
		n = ((n * 10) + digits[str[i...(i + 1)]])
		i = (i + 1)
	end
	if neg
		n = (-n)
	end
	return n
end

def isDigits(s)
	if ((s).length == 0)
		return false
	end
	i = 0
	while (i < (s).length)
		ch = _sliceString(s, i, (i + 1))
		if ((ch < "0") || (ch > "9"))
			return false
		end
		i = (i + 1)
	end
	return true
end

def readTable(table)
	toks = fields(table)
	cmds = []
	mins = []
	i = 0
	while (i < (toks).length)
		cmd = _indexString(toks, i)
		minlen = (cmd).length
		i = (i + 1)
		if ((i < (toks).length) && isDigits(_indexString(toks, i)))
			num = parseIntStr(_indexString(toks, i))
			if ((num >= 1) && (num < (cmd).length))
				minlen = num
				i = (i + 1)
			end
		end
		cmds = (cmds + [cmd])
		mins = (mins + [minlen])
	end
	return {"commands" => cmds, "mins" => mins}
end

def validate(commands, mins, words)
	results = []
	wi = 0
	while (wi < (words).length)
		w = words[wi]
		found = false
		wlen = (w).length
		ci = 0
		while (ci < (commands).length)
			cmd = commands[ci]
			if (((mins[ci] != 0) && (wlen >= mins[ci])) && (wlen <= (cmd).length))
				c = (cmd).to_s.upcase
				ww = (w).to_s.upcase
				if (_sliceString(c, 0, wlen) == ww)
					results = (results + [c])
					found = true
					break
				end
			end
			ci = (ci + 1)
		end
		if (!found)
			results = (results + ["*error*"])
		end
		wi = (wi + 1)
	end
	return results
end

def main()
	table = (((((((("" + "add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1  Schange  Cinsert 2  Clast 3 ") + "compress 4 copy 2 count 3 Coverlay 3 cursor 3  delete 3 Cdelete 2  down 1  duplicate ") + "3 xEdit 1 expand 3 extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2 ") + "forward 2  get  help 1 hexType 4  input 1 powerInput 3  join 1 split 2 spltJOIN load ") + "locate 1 Clocate 2 lowerCase 3 upperCase 3 Lprefix 2  macro  merge 2 modify 3 move 2 ") + "msg  next 1 overlay 1 parse preserve 4 purge 3 put putD query 1 quit  read recover 3 ") + "refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4 rgtLEFT right 2 left ") + "2  save  set  shift 2  si  sort  sos  stack 3 status 4 top  transfer 3  type 1  up 1 ")
	sentence = "riG   rePEAT copies  put mo   rest    types   fup.    6\npoweRin"
	tbl = readTable(table)
	commands = tbl["commands"]
	mins = tbl["mins"]
	words = fields(sentence)
	results = validate(commands, mins, words)
	out1 = "user words:"
	k = 0
	while (k < (words).length)
		out1 = (out1 + " ")
		if (k < ((words).length - 1))
			out1 = (out1 + padRight(_indexString(words, k), (_indexString(results, k)).length))
		else
			out1 = (out1 + _indexString(words, k))
		end
		k = (k + 1)
	end
	puts(out1)
	puts(("full words: " + (results).map(&:to_s).join(" ")))
end

main()
