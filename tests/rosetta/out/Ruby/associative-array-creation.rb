def removeKey(m, k)
	out = {}
	m.each do |key|
		if (key != k)
			out[key] = m[key]
		end
	end
	return out
end

def main()
	x = nil
	x = {}
	x["foo"] = 3
	y1 = x["bar"]
	ok = (x.to_h.key?("bar"))
	puts(y1.inspect)
	puts(ok)
	x = removeKey(x, "foo")
	x = {"foo" => 2, "bar" => 42, "baz" => (-1)}
	puts([x["foo"], x["bar"], x["baz"]].join(" "))
end

main()
