Counter = Struct.new(:n, keyword_init: true)

def inc(c)
	c.n = (c.n + 1)
end

$c = Counter.new(n: 0)
inc($c)
puts($c.n)
