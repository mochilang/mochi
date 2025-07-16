def doIt(p)
	b = 0
	if (p.include?("b"))
		b = p["b"]
	end
	return ((p["a"] + b) + p["c"])
end

def main()
	p = {}
	p["a"] = 1
	p["c"] = 9
	puts((doIt(p)).to_s)
end

main()
