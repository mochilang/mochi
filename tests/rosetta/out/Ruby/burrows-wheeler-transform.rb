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

def contains(s, ch)
	i = 0
	while (i < (s).length)
		if (_sliceString(s, i, (i + 1)) == ch)
			return true
		end
		i = (i + 1)
	end
	return false
end

def sortStrings(xs)
	arr = xs
	n = (arr).length
	i = 0
	while (i < n)
		j = 0
		while (j < (n - 1))
			if (arr[j] > arr[(j + 1)])
				tmp = arr[j]
				arr[j] = arr[(j + 1)]
				arr[(j + 1)] = tmp
			end
			j = (j + 1)
		end
		i = (i + 1)
	end
	return arr
end

def bwt(s)
	if (contains(s, $stx) || contains(s, $etx))
		return {"err" => true, "res" => ""}
	end
	s = (($stx + s) + $etx)
	le = (s).length
	table = []
	i = 0
	while (i < le)
		rot = (_sliceString(s, i, le) + _sliceString(s, 0, i))
		table = (table + [rot])
		i = (i + 1)
	end
	table = sortStrings(table)
	last = ""
	i = 0
	while (i < le)
		last = (last + _sliceString(table[i], (le - 1), le))
		i = (i + 1)
	end
	return {"err" => false, "res" => last}
end

def ibwt(r)
	le = (r).length
	table = []
	i = 0
	while (i < le)
		table = (table + [""])
		i = (i + 1)
	end
	n = 0
	while (n < le)
		i = 0
		while (i < le)
			table[i] = (_sliceString(r, i, (i + 1)) + table[i])
			i = (i + 1)
		end
		table = sortStrings(table)
		n = (n + 1)
	end
	i = 0
	while (i < le)
		if (_sliceString(table[i], (le - 1), le) == $etx)
			return _sliceString(table[i], 1, (le - 1))
		end
		i = (i + 1)
	end
	return ""
end

def makePrintable(s)
	out = ""
	i = 0
	while (i < (s).length)
		ch = _sliceString(s, i, (i + 1))
		if (ch == $stx)
			out = (out + "^")
		elsif (ch == $etx)
			out = (out + "|")
		else
			out = (out + ch)
		end
		i = (i + 1)
	end
	return out
end

def main()
	examples = ["banana", "appellee", "dogwood", "TO BE OR NOT TO BE OR WANT TO BE OR NOT?", "SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES", "\x02ABC\x03"]
	examples.each do |t|
		puts(makePrintable(t))
		res = bwt(t)
		if res["err"]
			puts(" --> ERROR: String can't contain STX or ETX")
			puts(" -->")
		else
			enc = res["res"]
			puts((" --> " + makePrintable(enc)))
			r = ibwt(enc)
			puts((" --> " + r))
		end
		puts("")
	end
end

$stx = "\x02"
$etx = "\x03"
main()
