def strdup(s)
	return (s + "")
end

def main()
	go1 = "hello C"
	c2 = strdup(go1)
	puts(c2)
end

main()
