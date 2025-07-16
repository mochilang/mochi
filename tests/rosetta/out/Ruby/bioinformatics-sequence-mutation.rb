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

def randInt(s, n)
	_next = ((((s * 1664525) + 1013904223)) % 2147483647)
	return [_next, (_next % n)]
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

def makeSeq(s, le)
	bases = "ACGT"
	out = ""
	i = 0
	while (i < le)
		r = randInt(s, 4)
		s = r[0]
		idx = r[1]
		out = (out + _sliceString(bases, idx, (idx + 1)))
		i = (i + 1)
	end
	return [s, out]
end

def mutate(s, dna, w)
	bases = "ACGT"
	le = (dna).length
	r = randInt(s, le)
	s = r[0]
	p = r[1]
	r = randInt(s, 300)
	s = r[0]
	x = r[1]
	arr = []
	i = 0
	while (i < le)
		arr = (arr + [_sliceString(dna, i, (i + 1))])
		i = (i + 1)
	end
	if (x < w[0])
		r = randInt(s, 4)
		s = r[0]
		idx = r[1]
		b = _sliceString(bases, idx, (idx + 1))
		puts((((((("  Change @" + padLeft((p).to_s, 3)) + " '") + arr[p]) + "' to '") + b) + "'"))
		arr[p] = b
	elsif (x < (w[0] + w[1]))
		puts((((("  Delete @" + padLeft((p).to_s, 3)) + " '") + arr[p]) + "'"))
		j = p
		while (j < ((arr).length - 1))
			arr[j] = arr[(j + 1)]
			j = (j + 1)
		end
		arr = arr[0...((arr).length - 1)]
	else
		r = randInt(s, 4)
		s = r[0]
		idx2 = r[1]
		b = _sliceString(bases, idx2, (idx2 + 1))
		arr = (arr + [""])
		j = ((arr).length - 1)
		while (j > p)
			arr[j] = arr[(j - 1)]
			j = (j - 1)
		end
		puts((((("  Insert @" + padLeft((p).to_s, 3)) + " '") + b) + "'"))
		arr[p] = b
	end
	out = ""
	i = 0
	while (i < (arr).length)
		out = (out + arr[i])
		i = (i + 1)
	end
	return [s, out]
end

def prettyPrint(dna, rowLen)
	puts("SEQUENCE:")
	le = (dna).length
	i = 0
	while (i < le)
		k = (i + rowLen)
		if (k > le)
			k = le
		end
		puts(((padLeft((i).to_s, 5) + ": ") + dna[i...k]))
		i = (i + rowLen)
	end
	a = 0
	c = 0
	g = 0
	t = 0
	idx = 0
	while (idx < le)
		ch = _sliceString(dna, idx, (idx + 1))
		if (ch == "A")
			a = (a + 1)
		else
			if (ch == "C")
				c = (c + 1)
			else
				if (ch == "G")
					g = (g + 1)
				else
					if (ch == "T")
						t = (t + 1)
					end
				end
			end
		end
		idx = (idx + 1)
	end
	puts("")
	puts("BASE COUNT:")
	puts(("    A: " + padLeft((a).to_s, 3)))
	puts(("    C: " + padLeft((c).to_s, 3)))
	puts(("    G: " + padLeft((g).to_s, 3)))
	puts(("    T: " + padLeft((t).to_s, 3)))
	puts("    ------")
	puts(("    Σ: " + (le).to_s))
	puts("    ======")
end

def wstring(w)
	return (((((("  Change: " + (w[0]).to_s) + "\n  Delete: ") + (w[1]).to_s) + "\n  Insert: ") + (w[2]).to_s) + "\n")
end

def main()
	seed = 1
	res = makeSeq(seed, 250)
	seed = res[0]
	dna = res[1]
	prettyPrint(dna, 50)
	muts = 10
	w = [100, 100, 100]
	puts("\nWEIGHTS (ex 300):")
	puts(wstring(w))
	puts((("MUTATIONS (" + (muts).to_s) + "):"))
	i = 0
	while (i < muts)
		res = mutate(seed, dna, w)
		seed = res[0]
		dna = res[1]
		i = (i + 1)
	end
	puts("")
	prettyPrint(dna, 50)
end

main()
