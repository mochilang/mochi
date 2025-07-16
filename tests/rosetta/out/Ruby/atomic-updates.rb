def randOrder(seed, n)
	_next = ((((seed * 1664525) + 1013904223)) % 2147483647)
	return [_next, (_next % n)]
end

def randChaos(seed, n)
	_next = ((((seed * 1103515245) + 12345)) % 2147483647)
	return [_next, (_next % n)]
end

def main()
	nBuckets = 10
	initialSum = 1000
	buckets = []
	(0...nBuckets).each do |i|
		buckets = (buckets + [0])
	end
	i = nBuckets
	dist = initialSum
	while (i > 0)
		v = (dist / i)
		i = (i - 1)
		buckets[i] = v
		dist = (dist - v)
	end
	tc0 = 0
	tc1 = 0
	total = 0
	nTicks = 0
	seedOrder = 1
	seedChaos = 2
	puts("sum  ---updates---    mean  buckets")
	t = 0
	while (t < 5)
		r = randOrder(seedOrder, nBuckets)
		seedOrder = r[0]
		b1 = r[1]
		b2 = (((b1 + 1)) % nBuckets)
		v1 = buckets[b1]
		v2 = buckets[b2]
		if (v1 > v2)
			a = ((((v1 - v2)) / 2))
			if (a > buckets[b1])
				a = buckets[b1]
			end
			buckets[b1] = (buckets[b1] - a)
			buckets[b2] = (buckets[b2] + a)
		else
			a = ((((v2 - v1)) / 2))
			if (a > buckets[b2])
				a = buckets[b2]
			end
			buckets[b2] = (buckets[b2] - a)
			buckets[b1] = (buckets[b1] + a)
		end
		tc0 = (tc0 + 1)
		r = randChaos(seedChaos, nBuckets)
		seedChaos = r[0]
		b1 = r[1]
		b2 = (((b1 + 1)) % nBuckets)
		r = randChaos(seedChaos, (buckets[b1] + 1))
		seedChaos = r[0]
		amt = r[1]
		if (amt > buckets[b1])
			amt = buckets[b1]
		end
		buckets[b1] = (buckets[b1] - amt)
		buckets[b2] = (buckets[b2] + amt)
		tc1 = (tc1 + 1)
		sum = 0
		idx = 0
		while (idx < nBuckets)
			sum = (sum + buckets[idx])
			idx = (idx + 1)
		end
		total = ((total + tc0) + tc1)
		nTicks = (nTicks + 1)
		puts((((((((((sum).to_s + " ") + (tc0).to_s) + " ") + (tc1).to_s) + " ") + ((total / nTicks)).to_s) + "  ") + (buckets).to_s))
		tc0 = 0
		tc1 = 0
		t = (t + 1)
	end
end

main()
