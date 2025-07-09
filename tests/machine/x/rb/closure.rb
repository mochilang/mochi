def makeAdder(n)
	return ->(x){ (x + n) }
end

$add10 = makeAdder(10)
puts($add10.call(7))
