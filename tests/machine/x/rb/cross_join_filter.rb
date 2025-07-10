require 'ostruct'

nums = [1, 2, 3]
letters = ["A", "B"]
pairs = (begin
	_res = []
	for n in nums
		for l in letters
			if ((n % 2) == 0)
				_res << OpenStruct.new(n: n, l: l)
			end
		end
	end
	_res
end)
puts("--- Even pairs ---")
pairs.each do |p|
	puts([p.n, p.l].join(" "))
end
