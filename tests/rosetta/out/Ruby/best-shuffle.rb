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

def nextRand(seed)
	return ((((seed * 1664525) + 1013904223)) % 2147483647)
end

def shuffleChars(s, seed)
	chars = []
	i = 0
	while (i < (s).length)
		chars = (chars + [_sliceString(s, i, (i + 1))])
		i = (i + 1)
	end
	sd = seed
	idx = ((chars).length - 1)
	while (idx > 0)
		sd = nextRand(sd)
		j = (sd % ((idx + 1)))
		tmp = chars[idx]
		chars[idx] = chars[j]
		chars[j] = tmp
		idx = (idx - 1)
	end
	res = ""
	i = 0
	while (i < (chars).length)
		res = (res + chars[i])
		i = (i + 1)
	end
	return [res, sd]
end

def bestShuffle(s, seed)
	r = shuffleChars(s, seed)
	t = r[0]
	sd = r[1]
	arr = []
	i = 0
	while (i < (t).length)
		arr = (arr + [_sliceString(t, i, (i + 1))])
		i = (i + 1)
	end
	i = 0
	while (i < (arr).length)
		j = 0
		while (j < (arr).length)
			if (((i != j) && (arr[i] != _sliceString(s, j, (j + 1)))) && (arr[j] != _sliceString(s, i, (i + 1))))
				tmp = arr[i]
				arr[i] = arr[j]
				arr[j] = tmp
				break
			end
			j = (j + 1)
		end
		i = (i + 1)
	end
	count = 0
	i = 0
	while (i < (arr).length)
		if (arr[i] == _sliceString(s, i, (i + 1)))
			count = (count + 1)
		end
		i = (i + 1)
	end
	out = ""
	i = 0
	while (i < (arr).length)
		out = (out + arr[i])
		i = (i + 1)
	end
	return [out, sd, count]
end

def main()
	ts = ["abracadabra", "seesaw", "elk", "grrrrrr", "up", "a"]
	seed = 1
	i = 0
	while (i < (ts).length)
		r = bestShuffle(ts[i], seed)
		shuf = r[0]
		seed = r[1]
		cnt = r[2]
		puts((((((ts[i] + " -> ") + shuf) + " (") + (cnt).to_s) + ")"))
		i = (i + 1)
	end
end

main()
