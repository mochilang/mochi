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

def isPrime(n)
	if (n < 2)
		return false
	end
	if ((n % 2) == 0)
		return (n == 2)
	end
	if ((n % 3) == 0)
		return (n == 3)
	end
	d = 5
	while ((d * d) <= n)
		if ((n % d) == 0)
			return false
		end
		d = (d + 2)
		if ((n % d) == 0)
			return false
		end
		d = (d + 4)
	end
	return true
end

def firstPrimeFactor(n)
	if (n == 1)
		return 1
	end
	if ((n % 3) == 0)
		return 3
	end
	if ((n % 5) == 0)
		return 5
	end
	inc = [4, 2, 4, 2, 4, 6, 2, 6]
	k = 7
	i = 0
	while ((k * k) <= n)
		if ((n % k) == 0)
			return k
		end
		k = (k + inc[i])
		i = (((i + 1)) % (inc).length)
	end
	return n
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

def padLeft(n, width)
	s = (n).to_s
	while ((s).length < width)
		s = (" " + s)
	end
	return s
end

def formatFloat(f, prec)
	s = (f).to_s
	idx = indexOf(s, ".")
	if (idx < 0)
		return s
	end
	need = ((idx + 1) + prec)
	if ((s).length > need)
		return _sliceString(s, 0, need)
	end
	return s
end

def main()
	blum = []
	counts = [0, 0, 0, 0]
	digits = [1, 3, 7, 9]
	i = 1
	bc = 0
	while true
		p = firstPrimeFactor(i)
		if ((p % 4) == 3)
			q = ((i / p))
			if (((q != p) && ((q % 4) == 3)) && isPrime(q))
				if (bc < 50)
					blum = (blum + [i])
				end
				d = (i % 10)
				if (d == 1)
					counts[0] = (counts[0] + 1)
				elsif (d == 3)
					counts[1] = (counts[1] + 1)
				elsif (d == 7)
					counts[2] = (counts[2] + 1)
				elsif (d == 9)
					counts[3] = (counts[3] + 1)
				end
				bc = (bc + 1)
				if (bc == 50)
					puts("First 50 Blum integers:")
					idx = 0
					while (idx < 50)
						line = ""
						j = 0
						while (j < 10)
							line = ((line + padLeft(blum[idx], 3)) + " ")
							idx = (idx + 1)
							j = (j + 1)
						end
						puts(_sliceString(line, 0, ((line).length - 1)))
					end
					break
				end
			end
		end
		if ((i % 5) == 3)
			i = (i + 4)
		else
			i = (i + 2)
		end
	end
end

main()
