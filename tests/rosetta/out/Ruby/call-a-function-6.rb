def bar(a, b, c)
	puts((((((a).to_s + ", ") + (b).to_s) + ", ") + (c).to_s))
end

def main()
	args = {}
	args["a"] = 3
	args["b"] = 2
	args["c"] = 1
	bar(args["a"], args["b"], args["c"])
end

main()
