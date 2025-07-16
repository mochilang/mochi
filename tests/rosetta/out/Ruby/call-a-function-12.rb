def mkAdd(a)
	return ->(b){ (a + b) }
end

def mysum(x, y)
	return (x + y)
end

def partialSum(x)
	return ->(y){ mysum(x, y) }
end

def main()
	add2 = mkAdd(2)
	add3 = mkAdd(3)
	puts((((add2.call(5)).to_s + " ") + (add3.call(6)).to_s))
	partial = partialSum(13)
	puts((partial.call(5)).to_s)
end

main()
