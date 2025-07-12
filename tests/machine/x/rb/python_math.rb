math = Struct.new(:pi, :e, :sqrt, :pow, :sin, :log).new(Math::PI, Math::E, Math.method(:sqrt), ->(x,y){ x**y }, Math.method(:sin), Math.method(:log))

$r = 3.0
$area = (math.pi * math.pow.call($r, 2.0))
$root = math.sqrt.call(49.0)
$sin45 = math.sin.call((math.pi / 4.0))
$log_e = math.log.call(math.e)
puts(["Circle area with r =", $r, "=>", $area].join(" "))
puts(["Square root of 49:", $root].join(" "))
puts(["sin(π/4):", $sin45].join(" "))
puts(["log(e):", $log_e].join(" "))
