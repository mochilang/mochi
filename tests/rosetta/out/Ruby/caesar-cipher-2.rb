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
		if (_sliceString(s, i, (i + 1)) == ch)
			return i
		end
		i = (i + 1)
	end
	return (-1)
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
	return "?"
end

def shiftRune(r, k)
	if ((r >= "a") && (r <= "z"))
		return chr(((((((ord(r) - 97) + k)) % 26)) + 97))
	end
	if ((r >= "A") && (r <= "Z"))
		return chr(((((((ord(r) - 65) + k)) % 26)) + 65))
	end
	return r
end

def encipher(s, k)
	out = ""
	i = 0
	while (i < (s).length)
		out = (out + shiftRune(s[i...(i + 1)], k))
		i = (i + 1)
	end
	return out
end

def decipher(s, k)
	return encipher(s, (((26 - (k % 26))) % 26))
end

def main()
	pt = "The five boxing wizards jump quickly"
	puts(("Plaintext: " + pt))
	[0, 1, 7, 25, 26].each do |key|
		if ((key < 1) || (key > 25))
			puts((("Key " + (key).to_s) + " invalid"))
			next
		end
		ct = encipher(pt, key)
		puts(("Key " + (key).to_s))
		puts(("  Enciphered: " + ct))
		puts(("  Deciphered: " + decipher(ct, key)))
	end
end

main()
