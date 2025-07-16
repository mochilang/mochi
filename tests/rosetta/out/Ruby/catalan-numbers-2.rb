def catalanRec(n)
	if (n == 0)
		return 1
	end
	t1 = (2 * n)
	t2 = (t1 - 1)
	t3 = (2 * t2)
	t5 = (t3 * catalanRec((n - 1)))
	return ((t5 / ((n + 1))))
end

def main()
	(1...16).each do |i|
		puts((catalanRec(i)).to_s)
	end
end

main()
