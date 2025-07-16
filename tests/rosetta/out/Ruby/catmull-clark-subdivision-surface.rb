def _sliceString(s, i, j)
  start = i
  finish = j
  chars = s.chars
  n = chars.length
  start += n if start < 0
  finish += n if finish < 0
  start = 0 if start < 0
  finish = n if finish > n
  finish = start if finish < start
  chars[start...finish].join
end

Point = Struct.new(:x, :y, :z, keyword_init: true)

Edge = Struct.new(:pn1, :pn2, :fn1, :fn2, :cp, keyword_init: true)

PointEx = Struct.new(:p, :n, keyword_init: true)

def indexOf(s, ch)
	i = 0
	while (i < (s).length)
		if (_sliceString(s, i, (i + 1)) == ch)
			return i
		end
		i = (i + 1)
	end
	return (-1)
end

def fmt4(x)
	y = (x * 10000.0)
	if (y >= 0)
		y = (y + 0.5)
	else
		y = (y - 0.5)
	end
	y = ((y) / 10000.0)
	s = (y).to_s
	dot = indexOf(s, ".")
	if (dot == (0 - 1))
		s = (s + ".0000")
	else
		decs = (((s).length - dot) - 1)
		if (decs > 4)
			s = _sliceString(s, 0, (dot + 5))
		else
			while (decs < 4)
				s = (s + "0")
				decs = (decs + 1)
			end
		end
	end
	if (x >= 0.0)
		s = (" " + s)
	end
	return s
end

def fmt2(n)
	s = (n).to_s
	if ((s).length < 2)
		return (" " + s)
	end
	return s
end

def sumPoint(p1, p2)
	return Point.new(x: (p1.x + p2.x), y: (p1.y + p2.y), z: (p1.z + p2.z))
end

def mulPoint(p, m)
	return Point.new(x: (p.x * m), y: (p.y * m), z: (p.z * m))
end

def divPoint(p, d)
	return mulPoint(p, (1.0 / d))
end

def centerPoint(p1, p2)
	return divPoint(sumPoint(p1, p2), 2.0)
end

def getFacePoints(points, faces)
	facePoints = []
	i = 0
	while (i < (faces).length)
		face = faces[i]
		fp = Point.new(x: 0.0, y: 0.0, z: 0.0)
		face.each do |idx|
			fp = sumPoint(fp, points[idx])
		end
		fp = divPoint(fp, ((face).length))
		facePoints = (facePoints + [fp])
		i = (i + 1)
	end
	return facePoints
end

def sortEdges(edges)
	res = []
	tmp = edges
	while ((tmp).length > 0)
		min = tmp[0]
		idx = 0
		j = 1
		while (j < (tmp).length)
			e = tmp[j]
			if ((e[0] < min[0]) || (((e[0] == min[0]) && (((e[1] < min[1]) || (((e[1] == min[1]) && (e[2] < min[2]))))))))
				min = e
				idx = j
			end
			j = (j + 1)
		end
		res = (res + [min])
		out = []
		k = 0
		while (k < (tmp).length)
			if (k != idx)
				out = (out + [tmp[k]])
			end
			k = (k + 1)
		end
		tmp = out
	end
	return res
end

def getEdgesFaces(points, faces)
	edges = []
	fnum = 0
	while (fnum < (faces).length)
		face = faces[fnum]
		numP = (face).length
		pi = 0
		while (pi < numP)
			pn1 = face[pi]
			pn2 = 0
			if (pi < (numP - 1))
				pn2 = face[(pi + 1)]
			else
				pn2 = face[0]
			end
			if (pn1 > pn2)
				tmpn = pn1
				pn1 = pn2
				pn2 = tmpn
			end
			edges = (edges + [[pn1, pn2, fnum]])
			pi = (pi + 1)
		end
		fnum = (fnum + 1)
	end
	edges = sortEdges(edges)
	merged = []
	idx = 0
	while (idx < (edges).length)
		e1 = edges[idx]
		if (idx < ((edges).length - 1))
			e2 = edges[(idx + 1)]
			if ((e1[0] == e2[0]) && (e1[1] == e2[1]))
				merged = (merged + [[e1[0], e1[1], e1[2], e2[2]]])
				idx = (idx + 2)
				next
			end
		end
		merged = (merged + [[e1[0], e1[1], e1[2], (-1)]])
		idx = (idx + 1)
	end
	edgesCenters = []
	merged.each do |me|
		p1 = points[me[0]]
		p2 = points[me[1]]
		cp = centerPoint(p1, p2)
		edgesCenters = (edgesCenters + [Edge.new(pn1: me[0], pn2: me[1], fn1: me[2], fn2: me[3], cp: cp)])
	end
	return edgesCenters
end

def getEdgePoints(points, edgesFaces, facePoints)
	edgePoints = []
	i = 0
	while (i < (edgesFaces).length)
		edge = edgesFaces[i]
		cp = edge.cp
		fp1 = facePoints[edge.fn1]
		fp2 = fp1
		if (edge.fn2 != (0 - 1))
			fp2 = facePoints[edge.fn2]
		end
		cfp = centerPoint(fp1, fp2)
		edgePoints = (edgePoints + [centerPoint(cp, cfp)])
		i = (i + 1)
	end
	return edgePoints
end

def getAvgFacePoints(points, faces, facePoints)
	numP = (points).length
	temp = []
	i = 0
	while (i < numP)
		temp = (temp + [PointEx.new(p: Point.new(x: 0.0, y: 0.0, z: 0.0), n: 0)])
		i = (i + 1)
	end
	fnum = 0
	while (fnum < (faces).length)
		fp = facePoints[fnum]
		faces[fnum].each do |pn|
			tp = temp[pn]
			temp[pn] = PointEx.new(p: sumPoint(tp.p, fp), n: (tp.n + 1))
		end
		fnum = (fnum + 1)
	end
	avg = []
	j = 0
	while (j < numP)
		tp = temp[j]
		avg = (avg + [divPoint(tp.p, tp.n)])
		j = (j + 1)
	end
	return avg
end

def getAvgMidEdges(points, edgesFaces)
	numP = (points).length
	temp = []
	i = 0
	while (i < numP)
		temp = (temp + [PointEx.new(p: Point.new(x: 0.0, y: 0.0, z: 0.0), n: 0)])
		i = (i + 1)
	end
	edgesFaces.each do |edge|
		cp = edge.cp
		arr = [edge.pn1, edge.pn2]
		arr.each do |pn|
			tp = temp[pn]
			temp[pn] = PointEx.new(p: sumPoint(tp.p, cp), n: (tp.n + 1))
		end
	end
	avg = []
	j = 0
	while (j < numP)
		tp = temp[j]
		avg = (avg + [divPoint(tp.p, tp.n)])
		j = (j + 1)
	end
	return avg
end

def getPointsFaces(points, faces)
	pf = []
	i = 0
	while (i < (points).length)
		pf = (pf + [0])
		i = (i + 1)
	end
	fnum = 0
	while (fnum < (faces).length)
		faces[fnum].each do |pn|
			pf[pn] = (pf[pn] + 1)
		end
		fnum = (fnum + 1)
	end
	return pf
end

def getNewPoints(points, pf, afp, ame)
	newPts = []
	i = 0
	while (i < (points).length)
		n = pf[i]
		m1 = (((n - 3.0)) / n)
		m2 = (1.0 / n)
		m3 = (2.0 / n)
		old = points[i]
		p1 = mulPoint(old, m1)
		p2 = mulPoint(afp[i], m2)
		p3 = mulPoint(ame[i], m3)
		newPts = (newPts + [sumPoint(sumPoint(p1, p2), p3)])
		i = (i + 1)
	end
	return newPts
end

def key(a, b)
	if (a < b)
		return (((a).to_s + ",") + (b).to_s)
	end
	return (((b).to_s + ",") + (a).to_s)
end

def cmcSubdiv(points, faces)
	facePoints = getFacePoints(points, faces)
	edgesFaces = getEdgesFaces(points, faces)
	edgePoints = getEdgePoints(points, edgesFaces, facePoints)
	avgFacePoints = getAvgFacePoints(points, faces, facePoints)
	avgMidEdges = getAvgMidEdges(points, edgesFaces)
	pointsFaces = getPointsFaces(points, faces)
	newPoints = getNewPoints(points, pointsFaces, avgFacePoints, avgMidEdges)
	facePointNums = []
	nextPoint = (newPoints).length
	facePoints.each do |fp|
		newPoints = (newPoints + [fp])
		facePointNums = (facePointNums + [nextPoint])
		nextPoint = (nextPoint + 1)
	end
	edgePointNums = {}
	idx = 0
	while (idx < (edgesFaces).length)
		e = edgesFaces[idx]
		newPoints = (newPoints + [edgePoints[idx]])
		edgePointNums[key(e.pn1, e.pn2)] = nextPoint
		nextPoint = (nextPoint + 1)
		idx = (idx + 1)
	end
	newFaces = []
	fnum = 0
	while (fnum < (faces).length)
		oldFace = faces[fnum]
		if ((oldFace).length == 4)
			a = oldFace[0]
			b = oldFace[1]
			c = oldFace[2]
			d = oldFace[3]
			fpnum = facePointNums[fnum]
			ab = edgePointNums[key(a, b)]
			da = edgePointNums[key(d, a)]
			bc = edgePointNums[key(b, c)]
			cd = edgePointNums[key(c, d)]
			newFaces = (newFaces + [[a, ab, fpnum, da]])
			newFaces = (newFaces + [[b, bc, fpnum, ab]])
			newFaces = (newFaces + [[c, cd, fpnum, bc]])
			newFaces = (newFaces + [[d, da, fpnum, cd]])
		end
		fnum = (fnum + 1)
	end
	return [newPoints, newFaces]
end

def formatPoint(p)
	return (((((("[" + fmt4(p.x)) + " ") + fmt4(p.y)) + " ") + fmt4(p.z)) + "]")
end

def formatFace(f)
	if ((f).length == 0)
		return "[]"
	end
	s = ("[" + fmt2(f[0]))
	i = 1
	while (i < (f).length)
		s = ((s + " ") + fmt2(f[i]))
		i = (i + 1)
	end
	s = (s + "]")
	return s
end

def main()
	inputPoints = [Point.new(x: (-1.0), y: 1.0, z: 1.0), Point.new(x: (-1.0), y: (-1.0), z: 1.0), Point.new(x: 1.0, y: (-1.0), z: 1.0), Point.new(x: 1.0, y: 1.0, z: 1.0), Point.new(x: 1.0, y: (-1.0), z: (-1.0)), Point.new(x: 1.0, y: 1.0, z: (-1.0)), Point.new(x: (-1.0), y: (-1.0), z: (-1.0)), Point.new(x: (-1.0), y: 1.0, z: (-1.0))]
	inputFaces = [[0, 1, 2, 3], [3, 2, 4, 5], [5, 4, 6, 7], [7, 0, 3, 5], [7, 6, 1, 0], [6, 1, 2, 4]]
	outputPoints = inputPoints
	outputFaces = inputFaces
	i = 0
	while (i < 1)
		res = cmcSubdiv(outputPoints, outputFaces)
		outputPoints = res[0]
		outputFaces = res[1]
		i = (i + 1)
	end
	outputPoints.each do |p|
		puts(formatPoint(p))
	end
	puts("")
	outputFaces.each do |f|
		puts(formatFace(f))
	end
end

main()
