def square_to_maps(square)
	emap = {}
	dmap = {}
	x = 0
	while (x < (square).length)
		row = square[x]
		y = 0
		while (y < (row).length)
			ch = row[y]
			emap[ch] = [x, y]
			dmap[(((x).to_s + ",") + (y).to_s)] = ch
			y = (y + 1)
		end
		x = (x + 1)
	end
	return {"e" => emap, "d" => dmap}
end

def remove_space(text, emap)
	s = (text).to_s.upcase
	out = ""
	i = 0
	while (i < (s).length)
		ch = s[i...(i + 1)]
		if ((ch != " ") && (emap.include?(ch)))
			out = (out + ch)
		end
		i = (i + 1)
	end
	return out
end

def encrypt(text, emap, dmap)
	text = remove_space(text, emap)
	row0 = []
	row1 = []
	i = 0
	while (i < (text).length)
		ch = text[i...(i + 1)]
		xy = emap[ch]
		row0 = (row0 + [xy[0]])
		row1 = (row1 + [xy[1]])
		i = (i + 1)
	end
	row1.each do |v|
		row0 = (row0 + [v])
	end
	res = ""
	j = 0
	while (j < (row0).length)
		key = (((row0[j]).to_s + ",") + (row0[(j + 1)]).to_s)
		res = (res + dmap[key])
		j = (j + 2)
	end
	return res
end

def decrypt(text, emap, dmap)
	text = remove_space(text, emap)
	coords = []
	i = 0
	while (i < (text).length)
		ch = text[i...(i + 1)]
		xy = emap[ch]
		coords = (coords + [xy[0]])
		coords = (coords + [xy[1]])
		i = (i + 1)
	end
	half = ((coords).length / 2)
	k1 = []
	k2 = []
	idx = 0
	while (idx < half)
		k1 = (k1 + [coords[idx]])
		idx = (idx + 1)
	end
	while (idx < (coords).length)
		k2 = (k2 + [coords[idx]])
		idx = (idx + 1)
	end
	res = ""
	j = 0
	while (j < half)
		key = (((k1[j]).to_s + ",") + (k2[j]).to_s)
		res = (res + dmap[key])
		j = (j + 1)
	end
	return res
end

def main()
	squareRosetta = [["A", "B", "C", "D", "E"], ["F", "G", "H", "I", "K"], ["L", "M", "N", "O", "P"], ["Q", "R", "S", "T", "U"], ["V", "W", "X", "Y", "Z"], ["J", "1", "2", "3", "4"]]
	squareWikipedia = [["B", "G", "W", "K", "Z"], ["Q", "P", "N", "D", "S"], ["I", "O", "A", "X", "E"], ["F", "C", "L", "U", "M"], ["T", "H", "Y", "V", "R"], ["J", "1", "2", "3", "4"]]
	textRosetta = "0ATTACKATDAWN"
	textWikipedia = "FLEEATONCE"
	textTest = "The invasion will start on the first of January"
	maps = square_to_maps(squareRosetta)
	emap = maps["e"]
	dmap = maps["d"]
	puts("from Rosettacode")
	puts(("original:\t " + textRosetta))
	s = encrypt(textRosetta, emap, dmap)
	puts(("codiert:\t " + s))
	s = decrypt(s, emap, dmap)
	puts(("and back:\t " + s))
	maps = square_to_maps(squareWikipedia)
	emap = maps["e"]
	dmap = maps["d"]
	puts("from Wikipedia")
	puts(("original:\t " + textWikipedia))
	s = encrypt(textWikipedia, emap, dmap)
	puts(("codiert:\t " + s))
	s = decrypt(s, emap, dmap)
	puts(("and back:\t " + s))
	maps = square_to_maps(squareWikipedia)
	emap = maps["e"]
	dmap = maps["d"]
	puts("from Rosettacode long part")
	puts(("original:\t " + textTest))
	s = encrypt(textTest, emap, dmap)
	puts(("codiert:\t " + s))
	s = decrypt(s, emap, dmap)
	puts(("and back:\t " + s))
end

main()
