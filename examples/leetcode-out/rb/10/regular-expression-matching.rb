def isMatch(s, p)
	m = (s).length
	n = (p).length
	memo = {}
	dfs = ->(i, j){
		key = ((i * ((n + 1))) + j)
		if (memo.include?(key))
			return memo[key]
		end
		if (j == n)
			return (i == m)
		end
		first = false
		if (i < m)
			if (((p[j] == s[i])) || ((p[j] == ".")))
				first = true
			end
		end
		ans = false
		if ((j + 1) < n)
			if (p[(j + 1)] == "*")
				if dfs.call(i, (j + 2))
					ans = true
				elsif (first && dfs.call((i + 1), j))
					ans = true
				end
			else
				if (first && dfs.call((i + 1), (j + 1)))
					ans = true
				end
			end
		else
			if (first && dfs.call((i + 1), (j + 1)))
				ans = true
			end
		end
		memo[key] = ans
		return ans
	}
	return dfs.call(0, 0)
end

