def parseBool(s)
	l = (s).to_s.downcase
	if (((((l == "1") || (l == "t")) || (l == true)) || (l == "yes")) || (l == "y"))
		return true
	end
	return false
end

def main()
	n = true
	puts(n)
	puts("bool")
	n = (!n)
	puts(n)
	x = 5
	y = 8
	puts(["x == y:", (x == y)].join(" "))
	puts(["x < y:", (x < y)].join(" "))
	puts("\nConvert String into Boolean Data type\n")
	str1 = "japan"
	puts(["Before :", "string"].join(" "))
	bolStr = parseBool(str1)
	puts(["After :", "bool"].join(" "))
end

main()
