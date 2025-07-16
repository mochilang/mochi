def zeroval(ival)
	x = ival
	x = 0
	return x
end

def zeroptr(ref)
	ref[0] = 0
end

def main()
	i = 1
	puts(("initial: " + (i).to_s))
	tmp = zeroval(i)
	puts(("zeroval: " + (i).to_s))
	box = [i]
	zeroptr(box)
	i = box[0]
	puts(("zeroptr: " + (i).to_s))
	puts("pointer: 0")
end

main()
