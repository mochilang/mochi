def sqrtApprox(x)
	guess = x
	i = 0
	while (i < 20)
		guess = (((guess + (x / guess))) / 2.0)
		i = (i + 1)
	end
	return guess
end

def makeSym(order, elements)
	return {"order" => order, "ele" => elements}
end

def unpackSym(m)
	n = m["order"]
	ele = m["ele"]
	mat = []
	idx = 0
	r = 0
	while (r < n)
		row = []
		c = 0
		while (c <= r)
			row = (row + [ele[idx]])
			idx = (idx + 1)
			c = (c + 1)
		end
		while (c < n)
			row = (row + [0.0])
			c = (c + 1)
		end
		mat = (mat + [row])
		r = (r + 1)
	end
	r = 0
	while (r < n)
		c = (r + 1)
		while (c < n)
			mat[r][c] = mat[c][r]
			c = (c + 1)
		end
		r = (r + 1)
	end
	return mat
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

def printSym(m)
	printMat(unpackSym(m))
end

def printLower(m)
	n = m["order"]
	ele = m["ele"]
	mat = []
	idx = 0
	r = 0
	while (r < n)
		row = []
		c = 0
		while (c <= r)
			row = (row + [ele[idx]])
			idx = (idx + 1)
			c = (c + 1)
		end
		while (c < n)
			row = (row + [0.0])
			c = (c + 1)
		end
		mat = (mat + [row])
		r = (r + 1)
	end
	printMat(mat)
end

def choleskyLower(a)
	n = a["order"]
	ae = a["ele"]
	le = []
	idx = 0
	while (idx < (ae).length)
		le = (le + [0.0])
		idx = (idx + 1)
	end
	row = 1
	col = 1
	dr = 0
	dc = 0
	i = 0
	while (i < (ae).length)
		e = ae[i]
		if (i < dr)
			d = (((e - le[i])) / le[dc])
			le[i] = d
			ci = col
			cx = dc
			j = (i + 1)
			while (j <= dr)
				cx = (cx + ci)
				ci = (ci + 1)
				le[j] = (le[j] + (d * le[cx]))
				j = (j + 1)
			end
			col = (col + 1)
			dc = (dc + col)
		else
			le[i] = sqrtApprox((e - le[i]))
			row = (row + 1)
			dr = (dr + row)
			col = 1
			dc = 0
		end
		i = (i + 1)
	end
	return {"order" => n, "ele" => le}
end

def demo(a)
	puts("A:")
	printSym(a)
	puts("L:")
	l = choleskyLower(a)
	printLower(l)
end

demo(makeSym(3, [25.0, 15.0, 18.0, (-5.0), 0.0, 11.0]))
demo(makeSym(4, [18.0, 22.0, 70.0, 54.0, 86.0, 174.0, 42.0, 62.0, 134.0, 106.0]))
