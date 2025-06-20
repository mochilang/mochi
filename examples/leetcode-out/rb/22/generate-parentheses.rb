def generateParenthesis(n)
	result = []
	backtrack = ->(current, open, close){
		if ((current).length == (n * 2))
			result = (result + [current])
		else
			if (open < n)
				backtrack.call((current + "("), (open + 1), close)
			end
			if (close < open)
				backtrack.call((current + ")"), open, (close + 1))
			end
		end
	}
	backtrack.call("", 0, 0)
	return result
end

