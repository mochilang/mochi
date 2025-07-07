Counter = Struct.new(:n)

def inc(c)
  c.n += 1
end

c = Counter.new(0)
inc(c)
puts c.n
