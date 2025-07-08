def outer(x)
	inner = ->(y){
		return (x + y)
	}
	return inner.call(5)
end

puts([outer(3)].join(" "))
