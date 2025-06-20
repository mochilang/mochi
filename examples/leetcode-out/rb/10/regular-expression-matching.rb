def isMatch(s, p)
	m = (s).length
	n = (p).length
	dp = []
	i = 0
	while (i <= m)
		row = []
		j = 0
		while (j <= n)
			row = (row + [false])
			j = (j + 1)
		end
		dp = (dp + [row])
		i = (i + 1)
	end
	dp[m][n] = true
	i2 = m
	while (i2 >= 0)
		j2 = (n - 1)
		while (j2 >= 0)
			first = false
			if (i2 < m)
				if (((p[j2] == s[i2])) || ((p[j2] == ".")))
					first = true
				end
			end
			star = false
			if ((j2 + 1) < n)
				if (p[(j2 + 1)] == "*")
					star = true
				end
			end
			if star
				ok = false
				if dp[i2][(j2 + 2)]
					ok = true
				else
					if first
						if dp[(i2 + 1)][j2]
							ok = true
						end
					end
				end
				dp[i2][j2] = ok
			else
				ok = false
				if first
					if dp[(i2 + 1)][(j2 + 1)]
						ok = true
					end
				end
				dp[i2][j2] = ok
			end
			j2 = (j2 - 1)
		end
		i2 = (i2 - 1)
	end
	return dp[0][0]
end

