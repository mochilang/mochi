require 'ostruct'

$nums = [1, 2]
$letters = ["A", "B"]
$bools = [true, false]
$combos = (begin
	_res = []
	for n in $nums
		for l in $letters
			for b in $bools
				_res << OpenStruct.new(n: n, l: l, b: b)
			end
		end
	end
	_res
end)
puts("--- Cross Join of three lists ---")
for c in $combos
	puts([c.n, c.l, c.b].join(" "))
end
