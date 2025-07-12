$r = 3.0
$area = (math.pi * math.pow.call($r, 2.0))
$root = math.sqrt.call(49.0)
$sin45 = math.sin.call((math.pi / 4.0))
$log_e = math.log.call(math.e)
puts(["Circle area with r =", $r, "=>", $area].join(" "))
puts(["Square root of 49:", method(:$root)].join(" "))
puts(["sin(π/4):", method(:$sin45)].join(" "))
puts(["log(e):", method(:$log_e)].join(" "))
