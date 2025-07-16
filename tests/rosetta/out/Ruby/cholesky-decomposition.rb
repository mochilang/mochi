def sqrtApprox(x)
	guess = x
	i = 0
	while (i < 20)
		guess = (((guess + (x / guess))) / 2.0)
		i = (i + 1)
	end
	return guess
end

def cholesky(a)
	n = (a).length
	l = []
	i = 0
	while (i < n)
		row = []
		j = 0
		while (j < n)
			row = (row + [0.0])
			j = (j + 1)
		end
		l = (l + [row])
		i = (i + 1)
	end
	i = 0
	while (i < n)
		j = 0
		while (j <= i)
			sum = a[i][j]
			k = 0
			while (k < j)
				sum = (sum - (l[i][k] * l[j][k]))
				k = (k + 1)
			end
			if (i == j)
				l[i][j] = sqrtApprox(sum)
			else
				l[i][j] = (sum / l[j][j])
			end
			j = (j + 1)
		end
		i = (i + 1)
	end
	return l
end

def printMat(m)
	i = 0
	while (i < (m).length)
		line = ""
		j = 0
		while (j < (m[i]).length)
			line = (line + (m[i][j]).to_s)
			if (j < ((m[i]).length - 1))
				line = (line + " ")
			end
			j = (j + 1)
		end
		puts(line)
		i = (i + 1)
	end
end

def demo(a)
	puts("A:")
	printMat(a)
	l = cholesky(a)
	puts("L:")
	printMat(l)
end

demo([[25.0, 15.0, (-5.0)], [15.0, 18.0, 0.0], [(-5.0), 0.0, 11.0]])
demo([[18.0, 22.0, 54.0, 42.0], [22.0, 70.0, 86.0, 62.0], [54.0, 86.0, 174.0, 134.0], [42.0, 62.0, 134.0, 106.0]])
