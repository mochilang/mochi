def Node(data)
	return {"Data" => data, "Balance" => 0, "Link" => [nil, nil]}
end

def getLink(n, dir)
	return (n["Link"])[dir]
end

def setLink(n, dir, v)
	links = n["Link"]
	links[dir] = v
	n["Link"] = links
end

def opp(dir)
	return (1 - dir)
end

def single(root, dir)
	tmp = getLink(root, opp(dir))
	setLink(root, opp(dir), getLink(tmp, dir))
	setLink(tmp, dir, root)
	return tmp
end

def double(root, dir)
	tmp = getLink(getLink(root, opp(dir)), dir)
	setLink(getLink(root, opp(dir)), dir, getLink(tmp, opp(dir)))
	setLink(tmp, opp(dir), getLink(root, opp(dir)))
	setLink(root, opp(dir), tmp)
	tmp = getLink(root, opp(dir))
	setLink(root, opp(dir), getLink(tmp, dir))
	setLink(tmp, dir, root)
	return tmp
end

def adjustBalance(root, dir, bal)
	n = getLink(root, dir)
	nn = getLink(n, opp(dir))
	if (nn["Balance"] == 0)
		root["Balance"] = 0
		n["Balance"] = 0
	elsif (nn["Balance"] == bal)
		root["Balance"] = (-bal)
		n["Balance"] = 0
	else
		root["Balance"] = 0
		n["Balance"] = bal
	end
	nn["Balance"] = 0
end

def insertBalance(root, dir)
	n = getLink(root, dir)
	bal = ((2 * dir) - 1)
	if (n["Balance"] == bal)
		root["Balance"] = 0
		n["Balance"] = 0
		return single(root, opp(dir))
	end
	adjustBalance(root, dir, bal)
	return double(root, opp(dir))
end

def insertR(root, data)
	if (root == nil)
		return {"node" => Node(data), "done" => false}
	end
	node = root
	dir = 0
	if ((node["Data"]) < data)
		dir = 1
	end
	r = insertR(getLink(node, dir), data)
	setLink(node, dir, r["node"])
	if r["done"]
		return {"node" => node, "done" => true}
	end
	node["Balance"] = ((node["Balance"]) + (((2 * dir) - 1)))
	if (node["Balance"] == 0)
		return {"node" => node, "done" => true}
	end
	if ((node["Balance"] == 1) || (node["Balance"] == ((-1))))
		return {"node" => node, "done" => false}
	end
	return {"node" => insertBalance(node, dir), "done" => true}
end

def Insert(tree, data)
	r = insertR(tree, data)
	return r["node"]
end

def removeBalance(root, dir)
	n = getLink(root, opp(dir))
	bal = ((2 * dir) - 1)
	if (n["Balance"] == ((-bal)))
		root["Balance"] = 0
		n["Balance"] = 0
		return {"node" => single(root, dir), "done" => false}
	end
	if (n["Balance"] == bal)
		adjustBalance(root, opp(dir), ((-bal)))
		return {"node" => double(root, dir), "done" => false}
	end
	root["Balance"] = (-bal)
	n["Balance"] = bal
	return {"node" => single(root, dir), "done" => true}
end

def removeR(root, data)
	if (root == nil)
		return {"node" => nil, "done" => false}
	end
	node = root
	if ((node["Data"]) == data)
		if (getLink(node, 0) == nil)
			return {"node" => getLink(node, 1), "done" => false}
		end
		if (getLink(node, 1) == nil)
			return {"node" => getLink(node, 0), "done" => false}
		end
		heir = getLink(node, 0)
		while (getLink(heir, 1) != nil)
			heir = getLink(heir, 1)
		end
		node["Data"] = heir["Data"]
		data = heir["Data"]
	end
	dir = 0
	if ((node["Data"]) < data)
		dir = 1
	end
	r = removeR(getLink(node, dir), data)
	setLink(node, dir, r["node"])
	if r["done"]
		return {"node" => node, "done" => true}
	end
	node["Balance"] = (((node["Balance"]) + 1) - (2 * dir))
	if ((node["Balance"] == 1) || (node["Balance"] == ((-1))))
		return {"node" => node, "done" => true}
	end
	if (node["Balance"] == 0)
		return {"node" => node, "done" => false}
	end
	return removeBalance(node, dir)
end

def Remove(tree, data)
	r = removeR(tree, data)
	return r["node"]
end

def indentStr(n)
	s = ""
	i = 0
	while (i < n)
		s = (s + " ")
		i = (i + 1)
	end
	return s
end

def dumpNode(node, indent, comma)
	sp = indentStr(indent)
	if (node == nil)
		line = (sp + "null")
		if comma
			line = (line + ",")
		end
		puts(line)
	else
		puts((sp + "{"))
		puts((((indentStr((indent + 3)) + "\"Data\": ") + (node["Data"]).to_s) + ","))
		puts((((indentStr((indent + 3)) + "\"Balance\": ") + (node["Balance"]).to_s) + ","))
		puts((indentStr((indent + 3)) + "\"Link\": ["))
		dumpNode(getLink(node, 0), (indent + 6), true)
		dumpNode(getLink(node, 1), (indent + 6), false)
		puts((indentStr((indent + 3)) + "]"))
		_end = (sp + "}")
		if comma
			_end = (_end + ",")
		end
		puts(_end)
	end
end

def dump(node, indent)
	dumpNode(node, indent, false)
end

def main()
	tree = nil
	puts("Empty tree:")
	dump(tree, 0)
	puts("")
	puts("Insert test:")
	tree = Insert(tree, 3)
	tree = Insert(tree, 1)
	tree = Insert(tree, 4)
	tree = Insert(tree, 1)
	tree = Insert(tree, 5)
	dump(tree, 0)
	puts("")
	puts("Remove test:")
	tree = Remove(tree, 3)
	tree = Remove(tree, 1)
	t = tree
	t["Balance"] = 0
	tree = t
	dump(tree, 0)
end

main()
