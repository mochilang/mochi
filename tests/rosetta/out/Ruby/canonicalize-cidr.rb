def _joinStrings(parts, sep)
  raise 'join expects list' unless parts.is_a?(Array)
  parts.map(&:to_s).join(sep)
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
def _splitString(s, sep)
  raise 'split expects string' unless s.is_a?(String)
  s.split(sep)
end

def split(s, sep)
	parts = []
	cur = ""
	i = 0
	while (i < (s).length)
		if ((((sep).length > 0) && ((i + (sep).length) <= (s).length)) && (_sliceString(s, i, (i + (sep).length)) == sep))
			parts = (parts + [cur])
			cur = ""
			i = (i + (sep).length)
		else
			cur = (cur + s[i...(i + 1)])
			i = (i + 1)
		end
	end
	parts = (parts + [cur])
	return parts
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

def repeat(ch, n)
	out = ""
	i = 0
	while (i < n)
		out = (out + ch)
		i = (i + 1)
	end
	return out
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

def toBinary(n, bits)
	b = ""
	val = n
	i = 0
	while (i < bits)
		b = (((val % 2)).to_s + b)
		val = ((val / 2))
		i = (i + 1)
	end
	return b
end

def binToInt(bits)
	n = 0
	i = 0
	while (i < (bits).length)
		n = ((n * 2) + parseIntStr(bits[i...(i + 1)]))
		i = (i + 1)
	end
	return n
end

def padRight(s, width)
	out = s
	while ((out).length < width)
		out = (out + " ")
	end
	return out
end

def canonicalize(cidr)
	parts = _splitString(cidr, "/")
	dotted = parts[0]
	size = parseIntStr(parts[1])
	binParts = []
	_splitString(dotted, ".").each do |p|
		binParts = (binParts + [toBinary(parseIntStr(p), 8)])
	end
	binary = _joinStrings(binParts, "")
	binary = (binary[0...size] + repeat("0", (32 - size)))
	canonParts = []
	i = 0
	while (i < (binary).length)
		canonParts = (canonParts + [(binToInt(binary[i...(i + 8)])).to_s])
		i = (i + 8)
	end
	return ((_joinStrings(canonParts, ".") + "/") + parts[1])
end

$tests = ["87.70.141.1/22", "36.18.154.103/12", "62.62.197.11/29", "67.137.119.181/4", "161.214.74.21/24", "184.232.176.184/18"]
$tests.each do |t|
	puts(((padRight(t, 18) + " -> ") + canonicalize(t)))
end
