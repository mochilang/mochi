math = Struct.new(:pi, :e, :sqrt, :pow, :sin, :log).new(Math::PI, Math::E, Math.method(:sqrt), ->(x,y){ x**y }, Math.method(:sin), Math.method(:log))

puts(math.sqrt.call(16.0))
puts(math.pi)
