def printStat(fs, path)
	if (fs.include?(path))
		if fs[path]
			puts((path + " is a directory"))
		else
			puts((path + " is a file"))
		end
	else
		puts((("stat " + path) + ": no such file or directory"))
	end
end

def main()
	fs = {}
	fs["docs"] = true
	["input.txt", "/input.txt", "docs", "/docs"].each do |p|
		printStat(fs, p)
	end
end

main()
