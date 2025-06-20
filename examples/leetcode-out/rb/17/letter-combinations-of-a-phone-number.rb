def letterCombinations(digits)
	if ((digits).length == 0)
		return []
	end
	mapping = {"2" => ["a", "b", "c"], "3" => ["d", "e", "f"], "4" => ["g", "h", "i"], "5" => ["j", "k", "l"], "6" => ["m", "n", "o"], "7" => ["p", "q", "r", "s"], "8" => ["t", "u", "v"], "9" => ["w", "x", "y", "z"]}
	result = [""]
	for d in digits
		if (!((mapping.include?(d))))
			next
		end
		letters = mapping[d]
		_next = (begin
	_res = []
	for p in result
		for ch in letters
			_res << (p + ch)
		end
	end
	_res
end)
		result = _next
	end
	return result
end

