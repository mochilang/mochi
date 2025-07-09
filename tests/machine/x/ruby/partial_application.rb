def add(a, b)
	return (a + b)
end

add5 = ->(_t0){ add(5, _t0) }
puts([add5.call(3)].join(" "))
