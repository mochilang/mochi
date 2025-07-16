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

def padLeft(s, w)
	res = ""
	n = (w - (s).length)
	while (n > 0)
		res = (res + " ")
		n = (n - 1)
	end
	return (res + s)
end

def indexOfFrom(s, ch, start)
	i = start
	while (i < (s).length)
		if (_sliceString(s, i, (i + 1)) == ch)
			return i
		end
		i = (i + 1)
	end
	return (-1)
end

def containsStr(s, sub)
	i = 0
	sl = (s).length
	subl = (sub).length
	while (i <= (sl - subl))
		if (_sliceString(s, i, (i + subl)) == sub)
			return true
		end
		i = (i + 1)
	end
	return false
end

def distinct(slist)
	res = []
	slist.each do |s|
		found = false
		res.each do |r|
			if (r == s)
				found = true
				break
			end
		end
		if (!found)
			res = (res + [s])
		end
	end
	return res
end

def permutations(xs)
	if ((xs).length <= 1)
		return [xs]
	end
	res = []
	i = 0
	while (i < (xs).length)
		rest = []
		j = 0
		while (j < (xs).length)
			if (j != i)
				rest = (rest + [xs[j]])
			end
			j = (j + 1)
		end
		subs = permutations(rest)
		subs.each do |p|
			perm = [xs[i]]
			k = 0
			while (k < (p).length)
				perm = (perm + [p[k]])
				k = (k + 1)
			end
			res = (res + [perm])
		end
		i = (i + 1)
	end
	return res
end

def headTailOverlap(s1, s2)
	start = 0
	while true
		ix = indexOfFrom(s1, s2[0...1], start)
		if (ix == (0 - 1))
			return 0
		end
		start = ix
		if (_sliceString(s2, 0, ((s1).length - start)) == _sliceString(s1, start, (s1).length))
			return ((s1).length - start)
		end
		start = (start + 1)
	end
end

def deduplicate(slist)
	arr = distinct(slist)
	filtered = []
	i = 0
	while (i < (arr).length)
		s1 = arr[i]
		within = false
		j = 0
		while (j < (arr).length)
			if ((j != i) && containsStr(arr[j], s1))
				within = true
				break
			end
			j = (j + 1)
		end
		if (!within)
			filtered = (filtered + [s1])
		end
		i = (i + 1)
	end
	return filtered
end

def joinAll(ss)
	out = ""
	ss.each do |s|
		out = (out + s)
	end
	return out
end

def shortestCommonSuperstring(slist)
	ss = deduplicate(slist)
	shortest = joinAll(ss)
	perms = permutations(ss)
	idx = 0
	while (idx < (perms).length)
		perm = perms[idx]
		sup = perm[0]
		i = 0
		while (i < ((ss).length - 1))
			ov = headTailOverlap(perm[i], perm[(i + 1)])
			sup = (sup + _sliceString(perm[(i + 1)], ov, (perm[(i + 1)]).length))
			i = (i + 1)
		end
		if ((sup).length < (shortest).length)
			shortest = sup
		end
		idx = (idx + 1)
	end
	return shortest
end

def printCounts(seq)
	a = 0
	c = 0
	g = 0
	t = 0
	i = 0
	while (i < (seq).length)
		ch = _sliceString(seq, i, (i + 1))
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
		i = (i + 1)
	end
	total = (seq).length
	puts((("\nNucleotide counts for " + seq) + ":\n"))
	puts((padLeft("A", 10) + padLeft((a).to_s, 12)))
	puts((padLeft("C", 10) + padLeft((c).to_s, 12)))
	puts((padLeft("G", 10) + padLeft((g).to_s, 12)))
	puts((padLeft("T", 10) + padLeft((t).to_s, 12)))
	puts((padLeft("Other", 10) + padLeft(((total - ((((a + c) + g) + t)))).to_s, 12)))
	puts("  ____________________")
	puts((padLeft("Total length", 14) + padLeft((total).to_s, 8)))
end

def main()
	tests = [["TA", "AAG", "TA", "GAA", "TA"], ["CATTAGGG", "ATTAG", "GGG", "TA"], ["AAGAUGGA", "GGAGCGCAUC", "AUCGCAAUAAGGA"], ["ATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTAT", "GGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGT", "CTATGTTCTTATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA", "TGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC", "AACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTT", "GCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTC", "CGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTTCGATTCTGCTTATAACACTATGTTCT", "TGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC", "CGTAAAAAATTACAACGTCCTTTGGCTATCTCTTAAACTCCTGCTAAATGCTCGTGC", "GATGGAGCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTTCGATT", "TTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC", "CTATGTTCTTATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA", "TCTCTTAAACTCCTGCTAAATGCTCGTGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGA"]]
	tests.each do |seqs|
		scs = shortestCommonSuperstring(seqs)
		printCounts(scs)
	end
end

main()
