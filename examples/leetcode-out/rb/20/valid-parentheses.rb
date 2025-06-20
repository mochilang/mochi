def isValid(s)
	stack = []
	n = (s).length
	for i in 0...n
		c = s[i]
		if (c == "(")
			stack = (stack + [")"])
		elsif (c == "[")
			stack = (stack + ["]"])
		elsif (c == "{")
			stack = (stack + ["}"])
		else
			if ((stack).length == 0)
				return false
			end
			top = stack[((stack).length - 1)]
			if (top != c)
				return false
			end
			stack = stack[0...((stack).length - 1)]
		end
	end
	return ((stack).length == 0)
end

