# Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
def add(a, b)
	return (a + b)
end

$add5 = ->(_t0){ add(5, _t0) }
puts($add5.call(3))
