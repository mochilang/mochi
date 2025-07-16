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
		if (s[i] == ch)
			return i
		end
		i = (i + 1)
	end
	return (-1)
end

def parseIntStr(str)
	i = 0
	neg = false
	if (((str).length > 0) && (str[0] == "-"))
		neg = true
		i = 1
	end
	n = 0
	digits = {"0" => 0, "1" => 1, "2" => 2, "3" => 3, "4" => 4, "5" => 5, "6" => 6, "7" => 7, "8" => 8, "9" => 9}
	while (i < (str).length)
		n = ((n * 10) + digits[str[i]])
		i = (i + 1)
	end
	if neg
		n = (-n)
	end
	return n
end

def ord(ch)
	upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	lower = "abcdefghijklmnopqrstuvwxyz"
	idx = indexOf(upper, ch)
	if (idx >= 0)
		return (65 + idx)
	end
	idx = indexOf(lower, ch)
	if (idx >= 0)
		return (97 + idx)
	end
	if ((ch >= "0") && (ch <= "9"))
		return (48 + parseIntStr(ch))
	end
	if (ch == "+")
		return 43
	end
	if (ch == "/")
		return 47
	end
	if (ch == " ")
		return 32
	end
	if (ch == "=")
		return 61
	end
	return 0
end

def chr(n)
	upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	lower = "abcdefghijklmnopqrstuvwxyz"
	if ((n >= 65) && (n < 91))
		return _sliceString(upper, (n - 65), (n - 64))
	end
	if ((n >= 97) && (n < 123))
		return _sliceString(lower, (n - 97), (n - 96))
	end
	if ((n >= 48) && (n < 58))
		digits = "0123456789"
		return _sliceString(digits, (n - 48), (n - 47))
	end
	if (n == 43)
		return "+"
	end
	if (n == 47)
		return "/"
	end
	if (n == 32)
		return " "
	end
	if (n == 61)
		return "="
	end
	return "?"
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

def base64Encode(text)
	alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
	bin = ""
	text.each do |ch|
		bin = (bin + toBinary(ord(ch), 8))
	end
	while (((bin).length % 6) != 0)
		bin = (bin + "0")
	end
	out = ""
	i = 0
	while (i < (bin).length)
		chunk = _sliceString(bin, i, (i + 6))
		val = binToInt(chunk)
		out = (out + _sliceString(alphabet, val, (val + 1)))
		i = (i + 6)
	end
	pad = (((3 - (((text).length % 3)))) % 3)
	if (pad == 1)
		out = (_sliceString(out, 0, ((out).length - 1)) + "=")
	end
	if (pad == 2)
		out = (_sliceString(out, 0, ((out).length - 2)) + "==")
	end
	return out
end

def base64Decode(enc)
	alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
	bin = ""
	i = 0
	while (i < (enc).length)
		ch = enc[i]
		if (ch == "=")
			break
		end
		idx = indexOf(alphabet, ch)
		bin = (bin + toBinary(idx, 6))
		i = (i + 1)
	end
	out = ""
	i = 0
	while ((i + 8) <= (bin).length)
		chunk = _sliceString(bin, i, (i + 8))
		val = binToInt(chunk)
		out = (out + chr(val))
		i = (i + 8)
	end
	return out
end

$msg = "Rosetta Code Base64 decode data task"
puts(("Original : " + $msg))
$enc = base64Encode($msg)
puts(("\nEncoded  : " + $enc))
$dec = base64Decode($enc)
puts(("\nDecoded  : " + $dec))
